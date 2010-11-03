-module(egeoip_acl).
-export([
    ip_by_location/4,
    parse_file/1
]).

parse_file(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {IPs, LocNames} = process_file(File, [], [], []),
    {LocationsTree, _} = lists:foldl(
                        fun(Loc, {Acc, Num}) ->
                            {gb_trees:insert(Loc, Num, Acc), Num + 1}
                        end,
                        {gb_trees:empty(), 0}, LocNames),
    file:close(File),
    {BinaryIPs, Offsets} = ips_to_binary(IPs),
    {BinaryIPs, Offsets, LocationsTree}.

process_file(File, Locations, CurLoc, LocNames) ->
    case file:read_line(File) of
        {ok, <<"acl ", Rest/binary>>} ->
            process_file(File, Locations, [], [location(Rest) | LocNames]);
        {ok, <<"};", _Rest/binary>>} ->
            process_file(File, [CurLoc | Locations], [], LocNames);
        {ok, Data} ->
            process_file(File, Locations,
                    process_line(Data, CurLoc),
                    LocNames);
        eof ->
            {Locations, lists:reverse(LocNames)}
    end.

location(<<"\"", LocBin/binary>>) ->
    "\n{ \"" ++ Location = lists:reverse(binary_to_list(LocBin)),
    list_to_binary(lists:reverse(Location)).

process_line(<<>>, Acc) -> Acc;
process_line(<<"\n">>, Acc) -> Acc;
process_line(<<IP:5/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:6/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:7/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:8/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:9/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:10/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest);
process_line(<<IP:11/binary, ".0/24;", Rest/binary>>, Acc) ->
    reduce_ips(IP, Acc, Rest).

reduce_ips(IP, Acc, Rest) ->
    {A, B, C} = parse_ip(IP),
    NewAcc = case Acc of
        [] ->
            [{A, B, {C, C}}];
        [LastIP | T] ->
            case LastIP of
                {A, B, {S, E}} when E == C - 1 ->
                    [{A, B, {S, C}} | T];
                {_, _, {_, _}} ->
                    [{A, B, {C, C}}, LastIP | T]
            end
    end,
    process_line(Rest, NewAcc).

parse_ip(IP) ->
    list_to_tuple(lists:map(fun list_to_integer/1, string:tokens(binary_to_list(IP), "."))).

ips_to_binary(IPsGroups) ->
    BinGroups = lists:map(fun ips_group_to_binary/1, IPsGroups),
    {WholeBin, AllOffsets} = lists:foldl(
            fun(BinGroup, {Bin, Offsets}) ->
                {
                    <<Bin/binary, BinGroup/binary>>,
                    <<(byte_size(Bin)):20, Offsets/binary-unit:20>>
                }
            end,
            {<<>>, <<>>}, BinGroups),
    {WholeBin, <<(byte_size(WholeBin)):20, AllOffsets/binary-unit:20>>}.

ips_group_to_binary(IPs) ->
    lists:foldl(
            fun({A, B, {S, E}}, Acc) ->
                <<Acc/binary, A:8, B:8, S:8, E:8>>
            end,
            <<>>, IPs).

ip_by_location(IPs, Positions, LocsTree, Loc) when is_list(Loc) ->
    ip_by_location(IPs, Positions, LocsTree, list_to_binary(Loc));
ip_by_location(IPs, Positions, LocsTree, Loc) ->
    {value, Offset} = gb_trees:lookup(Loc, LocsTree),
    <<_:Offset/binary-unit:20, Start:20, End:20, _/binary-unit:20>> = Positions,
    ip_ranges(binary_part(IPs, {Start, End - Start}), []).

ip_ranges(<<>>, IPs) -> IPs;
ip_ranges(<<A:8, B:8, S:8, E:8, Rest/binary>>, IPs) ->
    ip_ranges(Rest, [{A, B, {S, E}} | IPs]).
