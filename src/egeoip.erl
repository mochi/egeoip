%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Geolocation by IP address.

-module(egeoip).
-author('bob@redivi.com').

-behaviour(gen_server).

%% record access API
-export([get/2]).
-export([record_fields/0]).

%% gen_server based API
-export([start/0, start/1, start_link/1, start_link/2, stop/0,
         lookup/1, lookup_pl/1, reload/0, reload/1, filename/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

%% in-process API
-export([new/1, new/0]).
-export([lookup/2]).


%% useful utility functions
-export([ip2long/1]).

%% little benchmark function
-export([bench/0, bench/1]).

-include("egeoip.hrl").

%% geoip record API

%% @type geoip_atom() = country_code | country_code3 | country_name |
%%                      region | city | postal_code | latitude | longitude |
%%                      area_code | dma_code
%% @type geoip_field() = geoip_atom | [geoip_atom()]

%% @spec get(R::geoip(), Field::geoip_field()) -> term()
%% @doc Get a field from the geoip record returned by lookup.

get(R, country_code) ->
    R#geoip.country_code;
get(R, country_code3) ->
    R#geoip.country_code3;
get(R, country_name) ->
    R#geoip.country_name;
get(R, region) ->
    R#geoip.region;
get(R, city) ->
    R#geoip.city;
get(R, postal_code) ->
    R#geoip.postal_code;
get(R, latitude) ->
    R#geoip.latitude;
get(R, longitude) ->
    R#geoip.longitude;
get(R, area_code) ->
    R#geoip.area_code;
get(R, dma_code) ->
    R#geoip.dma_code;
get(R, List) when is_list(List) ->
    [get(R, X) || X <- List].

%% server API

%% @spec reload() -> ok
%% @doc Reload the existing database in this process and then change the
%%      state of the running server.
reload() ->
    reload(filename()).

%% @spec reload(Path) -> ok
%% @doc Load the database at Path in this process and then change the
%%      state of the running server with the new database.
reload(FileName) ->
    case new(FileName) of
        {ok, NewState} ->
            Workers = egeoip_sup:worker_names(),
            [gen_server:call(W, {reload, NewState})  || W <- tuple_to_list(Workers)];
        Error ->
            Error
    end.

%% @spec start() -> ok
%% @doc Start the egeoip application with the default database.
start() ->
    application:start(egeoip).

%% @spec start(File) -> ok
%% @doc Start the egeoip application with the File as database.
start(File) ->
    application:load(egeoip),
    application:set_env(egeoip, dbfile, File),
    start().


%% @spec start_link(Name) -> {ok, Pid}
%% @doc Start the server using the default priv/GeoLitecity.dat.gz database.
%%      The process will be registered as Name
start_link(Name) ->
    start_link(Name, city).

%% @spec start_link(Name, Path) -> {ok, Pid}
%% @doc Start the server using the database at Path registered as Name.
start_link(Name, FileName) ->
    gen_server:start_link(
      {local, Name}, ?MODULE, FileName, []).

%% @spec stop() -> ok
%% @doc Stop the server.
stop() ->
    application:stop(egeoip).

%% @spec lookup(Address) -> geoip()
%% @doc Get a geoip() record for the given address. Fields can be obtained
%%      from the record using get/2.
lookup(Address) ->
    case whereis(egeoip) of
        undefined ->
            Worker = get_worker(Address),
            gen_server:call(Worker, {lookup, Address});
        Pid ->
            unregister(egeoip),
            register(egeoip_0, Pid),
            FileName = gen_server:call(Pid, filename),
            [egeoip_0 | Workers] = tuple_to_list(egeoip_sup:worker_names()),
            Specs = egeoip_sup:worker(Workers, FileName),
            lists:map(fun(Spec) ->
                              {ok, _Pid} = supervisor:start_child(egeoip_sup, Spec)
                      end, Specs),
            lookup(Address)
    end.


%% @spec lookup_pl(Address) -> geoip()
%% @doc Get a proplist version of a geoip() record for the given address.
lookup_pl(Address) ->
    case lookup(Address) of
        {ok, #geoip{} = R} ->
            E = record_info(fields, geoip),
            lists:zip(E, lists:map(fun ensure_binary_list/1,
                                   tl(tuple_to_list(R))));
        Other ->
            Other
    end.

%% @spec record_fields() -> Fields::list()
%% @doc Get an ordered list of the geoip record fields
record_fields() ->
    record_info(fields, geoip).

%% @spec filename() -> string()
%% @doc Get the database filename currently being used by the server.
filename() ->
    gen_server:call(element(1, egeoip_sup:worker_names()), filename).

%% gen_server callbacks

%% @spec init(Path) -> {ok, State}
%% @doc initialize the server with the database at Path.
init(FileName) ->
    new(FileName).

%% @spec handle_call(Msg, From, State) -> term()
%% @doc gen_server callback.
handle_call(What,From,State) ->
    try
        do_handle_call(What,From,State)
    catch
        _:R ->
            log_error([{handle_call,What},{error,R}]),
            {reply,{error,R},State}
    end.

do_handle_call({lookup, Address}, _From, State) ->
    Res = lookup(State, Address),
    {reply, Res, State};
do_handle_call({reload, NewState}, _From, _State) ->
    {reply, ok, NewState};
do_handle_call(filename, _From, State) ->
    {reply, State#geoipdb.filename, State}.

%% @spec handle_cast(Msg, State) -> term()
%% @doc gen_server callback.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(What, State) ->
    log_error([{handle_cast, What}]),
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server callback.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> NewState
%% @doc gen_server callback.
code_change(_OldVsn, State, _Extra) ->
    State.

%% @spec handle_info(Info, State) -> {noreply, State}
%% @doc gen_server callback.
handle_info(Info, State) ->
    log_error([{handle_info,Info}]),
    {noreply, State}.

%% Implementation
get_worker(Address) ->
    element(1 + erlang:phash2(Address) band 7,
            egeoip_sup:worker_names()).

log_error(Info) ->
    error_logger:info_report([?MODULE|Info]).

%% @spec new() -> {ok, geoipdb()}
%% @doc Create a new geoipdb database record using the default
%%      priv/GeoLiteCity.dat.gz database.
new() ->
    new(city).

%% @spec new(Path) -> {ok, geoipdb()}
%% @doc Create a new geoipdb database record using the database at Path.
new(city) ->
    new(default_db(["GeoIPCity.dat", "GeoLiteCity.dat"]));
new(Path) ->
    case filelib:is_file(Path) of
        true ->
            Data = load_file(Path),
            Max = ?STRUCTURE_INFO_MAX_SIZE,
            R = {ok, State} = read_structures(Path, Data, size(Data) - 3, Max),
            ok = check_state(State),
            R;
        false ->
	    {error, {geoip_db_not_found,Path}}
    end.

%% @spec lookup(D::geoipdb(), Addr) -> {ok, geoip()}
%% @doc Lookup a geoip record for Addr using the database D.
lookup(D, Addr) when is_list(Addr);
                     is_tuple(Addr);
                     is_binary(Addr) ->
    case ip2long(Addr) of
        {ok, Ip} ->
            lookup(D, Ip);
        Error ->
            Error
    end;
lookup(D, Addr) when is_integer(Addr) ->
    get_record(D, Addr).

default_db([]) ->
    not_found;
default_db([Path | Rest]) ->
    FullPath = priv_path([Path]),
    case lists:filter(fun filelib:is_file/1, [FullPath ++ ".gz", FullPath]) of
        [] ->
            default_db(Rest);
        [DbPath | _] ->
            DbPath
    end.

address_fast([N2, N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N2, N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case N0 - $0 of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast(L=[_N2, _N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast(L=[_N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast([N0], Num, 0) ->
    case N0 - $0 of
        N when N =< 255 ->
            Num bor N
    end.

%% @spec ip2long(Address) -> {ok, integer()}
%% @doc Convert an IP address from a string, IPv4 tuple or IPv6 tuple to the
%%      big endian integer representation.
ip2long(Address) when is_integer(Address) ->
    {ok, Address};
ip2long(Address) when is_list(Address) ->
    case catch address_fast(Address, 0, 24) of
        N when is_integer(N) ->
            {ok, N};
        _ ->
            case inet_parse:address(Address) of
                {ok, Tuple} ->
                    ip2long(Tuple);
                Error ->
                    Error
            end
    end;
ip2long({B3, B2, B1, B0}) ->
    {ok, (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0};
ip2long({W7, W6, W5, W4, W3, W2, W1, W0}) ->
    {ok, (W7 bsl 112) bor (W6 bsl 96) bor (W5 bsl 80) bor (W4 bsl 64) bor
         (W3 bsl 48) bor (W2 bsl 32) bor (W1 bsl 16) bor W0};
ip2long(<<Addr:32>>) ->
    {ok, Addr};
ip2long(<<Addr:128>>) ->
    {ok, Addr};
ip2long(_) ->
    {error, badmatch}.

get_record(D, Ip) ->
    case seek_country(D, Ip) of
        {ok, SeekCountry} ->
            get_record(D, Ip, SeekCountry);
        Error ->
            Error
    end.



read_structures(_Path, _Data, _, 0) ->
    {error, read_structures_depth_exceeded};
read_structures(Path, Data, Seek, N) ->
    <<_:Seek/binary, Delim:3/binary, _/binary>> = Data,
    case Delim of
        <<255, 255, 255>> ->
            <<_:Seek/binary, _:3/binary, DbType, _/binary>> = Data,
            Type = case DbType >= 106 of
                       true ->
                           DbType - 105;
                       false ->
                           DbType
                   end,
            Segments = case Type of
                           ?GEOIP_REGION_EDITION_REV0 ->
                               ?GEOIP_STATE_BEGIN_REV0;
                           ?GEOIP_REGION_EDITION_REV1 ->
                               ?GEOIP_STATE_BEGIN_REV1;
                           ?GEOIP_COUNTRY_EDITION ->
                               ?GEOIP_COUNTRY_BEGIN;
                           ?GEOIP_PROXY_EDITION ->
                               ?GEOIP_COUNTRY_BEGIN;
                           ?GEOIP_NETSPEED_EDITION ->
                               ?GEOIP_COUNTRY_BEGIN;
                           _ ->
                               read_segments(Type, Data, Seek + 4)
                       end,
            Length = case Type of
                         ?GEOIP_ORG_EDITION ->
                             ?ORG_RECORD_LENGTH;
                         ?GEOIP_ISP_EDITION ->
                             ?ORG_RECORD_LENGTH;
                         _ ->
                             ?STANDARD_RECORD_LENGTH
                     end,
            Rec = #geoipdb{type = Type,
                           segments = Segments,
                           record_length = Length,
                           data = Data,
                           filename = Path},
            {ok, Rec};
        _ ->
            read_structures(Path, Data, Seek - 1, N - 1)
    end.


get_record(D, _Ip, SeekCountry) ->
    Length = D#geoipdb.record_length,
    Segments = D#geoipdb.segments,
    Seek = SeekCountry + (((2 * Length) - 1) * Segments),
    Data = D#geoipdb.data,
    <<_:Seek/binary, CountryNum, _/binary>> = Data,
    Country = country_code(D, CountryNum),
    Country3 = country_code3(D, CountryNum),
    CountryName = country_name(D, CountryNum),
    {Region, Seek1} = until_null(Data, Seek + 1, 0),
    {City, Seek2} = until_null(Data, Seek1, 0),
    {Postal, Seek3} = until_null(Data, Seek2, 0),
    <<_:Seek3/binary, RawLat:24/little, RawLon:24/little, _/binary>> = Data,
    Lat = (RawLat / 10000) - 180,
    Lon = (RawLon / 10000) - 180,
    Type = D#geoipdb.type,
    {DmaCode, AreaCode} = get_record_ex(Type, Country, Data, Seek3 + 6),
    Record = #geoip{country_code = Country,
                    country_code3 = Country3,
                    country_name = CountryName,
                    region = Region,
                    city = City,
                    postal_code = Postal,
                    latitude = Lat,
                    longitude = Lon,
                    dma_code = DmaCode,
                    area_code = AreaCode},
    {ok, Record}.

get_record_ex(?GEOIP_CITY_EDITION_REV1, "US", Data, Seek) ->
    <<_:Seek/binary, Combo:24/little, _/binary>> = Data,
    {Combo div 1000, Combo rem 1000};
get_record_ex(_, _, _, _) ->
    {0, 0}.



seek_country(D, Ip) ->
    seek_country(D, Ip, 0, 31).

seek_country(_D, _Ip, _Offset, -1) ->
    {error, seek_country_depth_exceeded};
seek_country(D, Ip, Offset, Depth) ->
    RecordLength = D#geoipdb.record_length,
    RB = 8 * RecordLength,
    Seek = 2 * RecordLength * Offset,
    <<_:Seek/binary, X0:RB/little, X1:RB/little, _/binary>> = D#geoipdb.data,
    X = case (Ip band (1 bsl Depth)) of
            0 -> X0;
            _ -> X1
        end,
    case (X >= D#geoipdb.segments) of
        true ->
            {ok, X};
        false ->
            seek_country(D, Ip, X, Depth - 1)
    end.

until_null(Binary, Start, Index) ->
    Skip = Start + Index,
    <<_:Skip/binary, Byte, _/binary>> = Binary,
    case Byte of
        0 ->
            Length = Skip - Start,
            <<_:Start/binary, Result:Length/binary, _/binary>> = Binary,
            {Result, 1 + Skip};
        _ ->
            until_null(Binary, Start, 1 + Index)
    end.

check_state(D) ->
    true = (size(D#geoipdb.country_codes) =:= ?GEOIP_NUM_COUNTRIES),
    true = (size(D#geoipdb.country_codes3) =:= ?GEOIP_NUM_COUNTRIES),
    true = (size(D#geoipdb.country_names) =:= ?GEOIP_NUM_COUNTRIES),
    ok.

country_code(D, Number) ->
    try
        element(Number, D#geoipdb.country_codes)
    catch
        error:badarg -> ""
    end.

country_code3(D, Number) ->
    try
        element(Number, D#geoipdb.country_codes3)
    catch
        error:badarg -> ""
    end.

country_name(D, Number) ->
    try
        element(Number, D#geoipdb.country_names)
    catch
        error:badarg -> ""
    end.

read_segments(Type, Data, Seek) when Type == ?GEOIP_CITY_EDITION_REV0;
                                     Type == ?GEOIP_CITY_EDITION_REV1;
                                     Type == ?GEOIP_ORG_EDITION;
                                     Type == ?GEOIP_ISP_EDITION;
                                     Type == ?GEOIP_ASNUM_EDITION ->
    Bits = ?SEGMENT_RECORD_LENGTH * 8,
    <<_:Seek/binary, Segments:Bits/little, _/binary>> = Data,
    Segments.


priv_path(Components) ->
    AppDir = case code:which(?MODULE) of
                 cover_compiled -> "..";
                 F -> filename:dirname(filename:dirname(F))
             end,
    filename:join([AppDir, "priv" | Components]).

load_file(Path) ->
    case file:read_file(Path) of
        {ok, Raw} ->
            case filename:extension(Path) of
                ".gz" ->
                    zlib:gunzip(Raw);
                _ ->
                    Raw
            end
    end.

benchcall(Fun, 1) ->
    Fun();
benchcall(Fun, Times) ->
    Fun(),
    benchcall(Fun, Times - 1).

pytime({MegaSecs, Secs, MicroSecs}) ->
    (1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs).

bench(Count) ->
    SampleIPs = ["63.224.214.117",
                 "144.139.80.91",
                 "88.233.53.82",
                 "85.250.32.5",
                 "220.189.211.182",
                 "211.112.118.99",
                 "84.94.205.244",
                 "61.16.226.206",
                 "64.180.1.78",
                 "138.217.4.11"],
    StartParse = now(),
    benchcall(fun () -> [lookup(X) || X <- SampleIPs] end, Count),
    EndParse = now(),
    {parse_100k_addr, pytime(EndParse) - pytime(StartParse)}.

ensure_binary_list(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary_list(Other) ->
    Other.

bench() ->
    bench(10000).
