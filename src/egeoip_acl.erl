-module(egeoip_acl).
-export([parse_file/1,
         lookup/2]).

-ifdef(TEST).
-export([parse_ip24/1,
         parse_ips/1,
         parse_line/2]).
-endif.

%% @type ip24() = list() | binary() | {int(), int(), int()}
%% @type ip32() = list() | binary() | {int(), int(), int(), int()}
%% @type ip() = ip24() | ip32() | invalid_ip
%% @type acldb() = {binary(), binary(), tuple()}

%% @spec parse_file(string() | atom()) -> acldb()
%% @doc Parses an ACL file and converts it to an IP -> Zone index.
%%
%%  IPs DATABASE FORMAT
%%
%%  {IPsBlob, IPsIndex, Zones}
%%
%%  1. IPsBlob consists of buckets, each bucket is associated with some
%%     16-bit IP (M.N.*.*) and contains IP ranges associated with a zone hash:
%%
%%  StartIP_1:8, EndIP_1:8, Zone_1:16, StartIP_2:8, EndIP_2:8, Zone_2:16, ...
%%
%%      Thus, every IP range uses 4 bytes: 2 bytes for start and end of
%%      the range and 2 bytes for zone hash.
%%
%%  2. IPsIndex has 65536 parts associated with all possible 16-bit IPs
%%     (from 0.0.*.* to 255.255.*.*). Every part has 4 bytes:
%%
%%  BucketStart:16, BucketSize:16
%%
%%      BucketStart represents the offset of the bucket from PIsBlob associated
%%      with the target 16-bit IP, and BucketSize represents its size, in bytes.
%%
%%  3. Zones is a simple tuple of zones, associating zone number from IPsBlob's
%%     buckets with zone name.
%%
parse_file(Filename) ->
    RawLines = read_file(Filename),
    IPsByZone = parse_line(RawLines, []),
    {Zones, ZonesTree} = zones_index(IPsByZone),
    IPZoneKV = extract_ips(IPsByZone, ZonesTree),
    {IPsBlob, IPsIndex} = ips_index(IPZoneKV),
    {IPsBlob, IPsIndex, Zones}.

%% @spec lookup(ip(), acldb()) -> binary() | notfound | invalid_ip
%% @doc Searches for 24-bit or 32-bit IP in the database generated from ACL
%%      file. Returns a binary name of the zone or 'notfound' if there's no
%%      zone associated with the IP.
lookup(invalid_ip, _) -> invalid_ip;
lookup(<<Prefix:16/integer, C:8>>, {IPsBlob, IPsIndex, Zones}) ->
    IndexOffset = Prefix * 4,
    << _:(IndexOffset)/binary,
       BucketOffset:16/integer,
       BucketSize:16/integer,
       _/binary >> = IPsIndex,
    << _:(BucketOffset)/binary,
       Ranges:(BucketSize)/binary,
       _/binary >> = IPsBlob,
    case lookup_ip(C, Ranges) of
        V when is_integer(V) ->
            element(V, Zones);
        _ ->
            notfound
    end;
lookup(IP, Database) ->
    lookup(parse_ip(IP), Database).

parse_ip({A, B, C}) -> <<A:8, B:8, C:8>>;
parse_ip({A, B, C, _}) -> <<A:8, B:8, C:8>>;
parse_ip(<<A:8, B:8, C:8, _D:8>>) -> <<A:8, B:8, C:8>>;
parse_ip(IP) when is_binary(IP) ->
    parse_ip(binary_to_list(IP));
parse_ip(IP) when is_list(IP) ->
    case lists:map(
           fun list_to_integer/1,
           string:tokens(IP, ".")
          ) of
        [A, B, C] -> <<A:8, B:8, C:8>>;
        [A, B, C, _] -> <<A:8, B:8, C:8>>;
        _ -> invalid_ip
    end;
parse_ip(_) -> invalid_ip.

lookup_ip(_, <<>>) -> notfound;
lookup_ip(C, Bin) ->
    Size = byte_size(Bin) div 4,
    Left = Size div 2,
    Right = Size - Left - 1,
    << Prefix:Left/binary-unit:32,
       Start:8, End:8, ZNum:16/integer,
       Suffix:Right/binary-unit:32 >> = Bin,
    case C of
        C when C < Start ->
            lookup_ip(C, Prefix);
        C when C > End ->
            lookup_ip(C, Suffix);
        C when C >= Start, C =< End ->
            ZNum;
        C -> notfound
    end.

read_file(Filename) ->
    {ok, File} = file:open(Filename, [binary, read]),
    RawLines = read_line(File, []),
    file:close(File),
    RawLines.

read_line(File, Acc) ->
    case file:read_line(File) of
        {ok, Line} -> read_line(File, [Line | Acc]);
        eof -> lists:reverse(Acc)
    end.

parse_line([], Zones) -> Zones;
parse_line([<<"acl \"", Rest/binary>> | T], Zones) ->
    L = byte_size(Rest) - 4,
    <<Zone:L/binary, "\" {\n">> = Rest,
    parse_line(T, [{Zone, <<>>} | Zones]);
parse_line([<<"};\n">> | T], [{Zone, IPsBin} | Zones]) ->
    parse_line(T, [{Zone, parse_ips(IPsBin)} | Zones]);
parse_line([Line | T], [{Zone, IPs} | Zones]) when is_binary(IPs) ->
    parse_line(T, [{Zone, <<IPs/binary, Line/binary>>} | Zones]);
parse_line([_ | T], Zones) ->
    parse_line(T, Zones).

%% NOTE: We sort IPs here in order to be able to build Zone -> IP index
%%       easier in the future. Also, it will speed up the entire IPs list
%%       sorting.
parse_ips(IPsBin) ->
    reduce_ips(
      lists:sort(
        split_ips(IPsBin, [])
       ),
      []
     ).

split_ips(<<>>, Acc) -> Acc;
split_ips(<<"\n", Rest/binary>>, Acc) ->
    split_ips(Rest, Acc);
split_ips(<<IP:5/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:6/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:7/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:8/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:9/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:10/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]);
split_ips(<<IP:11/binary, ".0/24;", Rest/binary>>, Acc) ->
    split_ips(Rest, [parse_ip24(IP) | Acc]).

parse_ip24(IP) ->
    [A, B, C] = lists:map(
                  fun list_to_integer/1,
                  string:tokens(binary_to_list(IP), ".")
                 ),
    <<A:8,B:8,C:8>>.

reduce_ips([], Groups) -> Groups;
reduce_ips([<<A:8,B:8,C:8>> | IPs], [<<A:8,B:8,Start:8,End:8>> | Groups])
  when End =:= C - 1 ->
    reduce_ips(IPs, [<<A:8,B:8,Start:8,C:8>> | Groups]);
reduce_ips([<<A:8,B:8,C:8>> | IPs], Groups) ->
    reduce_ips(IPs, [<<A:8,B:8,C:8,C:8>> | Groups]).

zones_index(IPsByZones) ->
    ZonesList = [Zone || {Zone, _IPs} <- IPsByZones],
    {ZonesTree, _} = lists:foldl(
                       fun (Zone, {Acc, Num}) ->
                               {gb_trees:insert(Zone, Num, Acc), Num + 1}
                       end,
                       {gb_trees:empty(), 1}, ZonesList),
    {
      list_to_tuple(ZonesList),
      ZonesTree
    }.

extract_ips(IPsByZone, ZonesTree) ->
    lists:sort(lists:flatten([
                              begin
                                  {value, Num} = gb_trees:lookup(Zone, ZonesTree),
                                  [{IP, Num} || IP <- IPs]
                              end
                              || {Zone, IPs} <- IPsByZone
                             ])).

ips_index(IPZoneKV) ->
    IPGroups = group_ips(IPZoneKV, []),
    ZeroIPsIndex =  list_to_binary(
                      lists:duplicate(256 * 256, <<0:32/integer>>)
                     ),
    merge_ip_groups(IPGroups, <<>>, ZeroIPsIndex).

group_ips([], Groups) -> Groups;
group_ips([{<<Addr:16/integer, Start:8, End:8>>, Zone} | IPs],
          [<<Addr:16/integer, Rest/binary>> | Groups]) ->
    group_ips(IPs, [<<Addr:16/integer, Rest/binary, Start:8, End:8, Zone:16/integer>> | Groups]);
group_ips([{<<Addr:16/integer, Start:8, End:8>>, Zone} | IPs], Groups) ->
    group_ips(IPs, [<<Addr:16/integer, Start:8, End:8, Zone:16/integer>> | Groups]).

merge_ip_groups([], IPsBlob, IPsIndex) -> {IPsBlob, IPsIndex};
merge_ip_groups([<<Addr:16/integer,Ranges/binary>> | Groups], IPsBlob, IPsIndex) ->
    L = Addr * 4,
    <<Prefix:L/binary, 0:32/integer, Suffix/binary>> = IPsIndex,
    merge_ip_groups(Groups, <<IPsBlob/binary, Ranges/binary>>,
                    << Prefix/binary,
                       (byte_size(IPsBlob)):16/integer,
                       (byte_size(Ranges)):16/integer,
                       Suffix/binary >>
                   ).
