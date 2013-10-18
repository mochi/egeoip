-module(egeoip_tests).
-include_lib("eunit/include/eunit.hrl").
-include("egeoip.hrl").

address_fast_test_() ->
    [?_assertEqual(invalid_fast_address, egeoip:address_fast("1", 0, 24)),
     ?_assertEqual(16909060, egeoip:address_fast("1.2.3.4", 0, 24)),
     ?_assertEqual(3573612662, egeoip:address_fast("213.1.0.118", 0 , 24))].

run_test_() ->
    {inorder,
     {foreach,
      fun egeoip:start/0,
      fun(_) -> egeoip:stop() end,
      [{"egeoip_bench", fun egeoip_bench/0},
       {"egeoip", fun egeoip/0},
       {"egeoip_fail", fun egeoip_fail/0},
       {"egeoip_lookup", fun egeoip_lookup/0},
       {"egeoip_reserved", {generator, fun egeoip_reserved_gen/0}},
       {"country_test", {generator, fun country_test_gen/0}},
       {"country_test2", {generator, fun country_test2_gen/0}},
       {"country_test_countrydb", {generator, fun country_test_gen/0}},
       {"non_parallel", fun non_parallel/0}]}}.

run_countrydb_test_() ->
  {inorder,
     {foreach,
      fun() -> egeoip:start(country) end,
      fun(_) -> egeoip:stop() end,
      [{"egeoip_bench", fun egeoip_bench/0},
       {"egeoip", fun egeoip_countrydb/0},
       {"egeoip_lookup", fun egeoip_lookup/0},
       {"egeoip_reserved", {generator, fun egeoip_reserved_gen/0}},
       {"country_test2", {generator, fun country_test2_gen/0}},
       {"non_parallel", fun non_parallel/0}]}}.

egeoip_reserved_gen() ->
    %% We don't test all of them, just a few.
    [?_assertMatch(
        {ok, #geoip{country_code = "",
                    country_code3 = "",
                    country_name = "",
                    region = <<>>,
                    city = <<>>,
                    postal_code = <<>>,
                    area_code = 0,
                    dma_code = 0}},
       egeoip:lookup(Ip))
     || Ip <- ["0.0.0.1", "127.0.0.1", "10.0.0.1", "192.168.0.1"]].

egeoip_bench() ->
    ?assertMatch(
       {_, _},
       egeoip:bench(1)),
    ok.

egeoip() ->
    {ok, IpAddressLong} = egeoip:ip2long({207,145,216,106}),
    {ok, IpAddressLong} = egeoip:ip2long("207.145.216.106"),
    {ok, IpAddressLong} = egeoip:ip2long(<<207,145,216,106>>),
    {ok, R} = egeoip:lookup(IpAddressLong),
    #geoip{country_code = "US",
           country_code3 = "USA",
           country_name = "United States",
           region = <<"CA">>,
           _ = _} = R,
    %% This is the test IP that MaxMind uses
    {ok, R1} = egeoip:lookup("24.24.24.24"),
    #geoip{country_code = "US",
           country_code3 = "USA",
           country_name = "United States",
           region = <<"NY">>,
           _ = _} = R1.


egeoip_fail() ->
    ?assertMatch({ok, #geoip{country_code = "",
                             country_code3 = "",
                             country_name = "",
                             region = <<>>,
                             city = <<>>,
                             postal_code = <<>>,
                             area_code = 0,
                             dma_code = 0}}, egeoip:lookup("2")),
    ok.

%% countrydb doesn't provide region info
egeoip_countrydb() ->
    {ok, IpAddressLong} = egeoip:ip2long({207,145,216,106}),
    {ok, IpAddressLong} = egeoip:ip2long("207.145.216.106"),
    {ok, IpAddressLong} = egeoip:ip2long(<<207,145,216,106>>),
    {ok, R} = egeoip:lookup(IpAddressLong),
    #geoip{country_code = "US",
           country_code3 = "USA",
           country_name = "United States",
           _ = _} = R,
    %% This is the test IP that MaxMind uses
    {ok, R1} = egeoip:lookup("24.24.24.24"),
    #geoip{country_code = "US",
           country_code3 = "USA",
           country_name = "United States",
           _ = _} = R1.

egeoip_lookup() ->
    {ok, R1} = egeoip:lookup("24.24.24.24"),
    {ok, R2} = egeoip:lookup({24,24,24,24}),
    ?assertEqual(R1,R2).

non_parallel() ->
    %% recreate the non-parallelized version of egeoip and then verify
    %% that the upgrade works.
    Workers = [Egeoip | T] = tuple_to_list(egeoip_sup:worker_names()),
    %% Remove all worker processes except for the first one
    lists:map(fun(Worker) ->
                      ok = supervisor:terminate_child(egeoip_sup, Worker),
                      ok = supervisor:delete_child(egeoip_sup, Worker)
              end, T),
    Pid = whereis(Egeoip),
    unregister(Egeoip),
    register(egeoip, Pid),
    ?assert(Pid == whereis(egeoip)),
    [?assert(undefined == whereis(W)) || W <- Workers],
    %% Should upgrade when calling lookup
    {ok, _R} = egeoip:lookup("24.24.24.24"),
    ?assert(undefined == whereis(egeoip)),
    [?assertNot(undefined == whereis(W)) || W <- Workers].

no_egeoip_test() ->
    Lookup = {lookup, "24.24.24.24"},
    ?assertExit({noproc,{gen_server,call,[egeoip,Lookup]}},gen_server:call(egeoip, Lookup)).

country_test_gen() ->
    [{Ip ++ " -> " ++ CC ++ " / " ++ C3,
      ?_assertMatch(
         {ok, #geoip{country_code=CC, country_code3=C3}},
         egeoip:lookup(Ip))}
     || {Ip, CC, C3} <- country_test_data()].

country_test2_gen() ->
    [{Ip ++ " -> " ++ CC,
      ?_assertMatch(
         {ok, #geoip{country_code=CC}},
         egeoip:lookup(Ip))}
     || {Ip, CC} <- country_test2_data()].

country_test2_data() ->
    [{"212.118.5.94", "JO"},
     {"64.170.57.29", "US"},
     {"202.7.216.215", "AU"},
     {"1.2.3.4", "AU"},
     {"212.33.164.149", "SA"},
     {"68.96.110.210", "US"},
     {"213.166.131.168", "SA"},
     {"64.158.191.179", "US"},
     {"24.247.251.23", "US"},
     {"203.199.228.66", "IN"},
     {"195.14.141.225", "CY"},
     {"200.52.94.98", "MX"},
     {"203.197.187.193", "IN"},
     {"203.128.9.170", "PK"},
     {"144.106.240.140", "US"},
     {"195.248.180.102", "UA"},
     {"213.1.0.118", "GB"},
     {"64.255.148.52", "US"},
     {"12.78.124.119", "US"},
     {"212.68.224.183", "BE"},
     {"62.148.73.85", "PL"},
     {"203.146.135.180", "TH"},
     {"209.204.179.145", "US"},
     {"64.123.0.164", "US"},
     {"202.56.198.16", "IN"},
     {"61.0.94.172", "IN"},
     {"62.42.171.190", "ES"},
     {"192.117.245.177", "IL"},
     {"213.123.75.243", "GB"},
     {"80.56.171.62", "NL"}].

country_test_data() ->
    [{"216.236.135.152", "US", "USA"},
     {"192.106.51.100", "IT", "ITA"},
     {"147.251.48.1", "CZ", "CZE"},
     {"203.174.65.12", "JP", "JPN"},
     {"212.208.74.140", "FR", "FRA"},
     {"200.219.192.106", "BR", "BRA"},
     {"134.102.101.18", "DE", "DEU"},
     {"193.75.148.28", "BE", "BEL"},
     {"194.244.83.2", "IT", "ITA"},
     {"203.15.106.23", "AU", "AUS"},
     {"196.31.1.1", "ZA", "ZAF"},
     {"151.28.39.114", "IT", "ITA"},
     {"151.38.70.94", "IT", "ITA"},
     {"193.56.4.124", "FR", "FRA"},
     {"195.142.146.198", "TR", "TUR"},
     {"211.232.0.0", "KR", "KOR"},
     {"211.240.0.0", "KR", "KOR"},
     {"193.194.4.0", "MA", "MAR"},
     {"139.20.112.104", "DE", "DEU"},
     {"139.20.112.3", "DE", "DEU"},
     {"145.236.125.211", "HU", "HUN"},
     {"149.225.169.61", "DE", "DEU"},
     {"151.17.191.46", "IT", "ITA"},
     {"151.24.176.194", "IT", "ITA"},
     {"151.25.8.136", "IT", "ITA"},
     {"151.26.146.192", "IT", "ITA"},
     {"151.26.153.66", "IT", "ITA"},
     {"151.26.167.71", "IT", "ITA"},
     {"151.26.35.204", "IT", "ITA"},
     {"151.26.64.157", "IT", "ITA"},
     {"151.27.138.182", "IT", "ITA"},
     {"151.28.39.114", "IT", "ITA"},
     {"151.29.150.217", "IT", "ITA"},
     {"151.29.237.39", "IT", "ITA"},
     {"151.29.73.189", "IT", "ITA"},
     {"151.30.134.242", "IT", "ITA"},
     {"151.30.135.85", "IT", "ITA"},
     {"151.30.168.224", "IT", "ITA"},
     {"151.35.80.202", "IT", "ITA"},
     {"151.35.80.240", "IT", "ITA"},
     {"151.36.191.229", "IT", "ITA"},
     {"151.38.70.94", "IT", "ITA"},
     {"151.38.92.126", "IT", "ITA"},
     {"151.42.100.132", "IT", "ITA"},
     {"151.42.169.71", "IT", "ITA"},
     {"193.56.4.124", "FR", "FRA"},
     {"195.142.146.198", "TR", "TUR"},
     {"195.142.49.205", "TR", "TUR"},
     {"202.247.74.18", "JP", "JPN"},
     {"202.247.74.71", "JP", "JPN"},
     {"202.247.74.81", "JP", "JPN"},
     {"202.247.74.88", "JP", "JPN"},
     {"203.242.239.188", "KR", "KOR"},
     {"203.174.65.12", "JP", "JPN"},
     {"212.208.74.140", "FR", "FRA"},
     {"200.219.192.106", "BR", "BRA"},
     {"202.53.254.193", "ID", "IDN"},
     {"12.168.0.0", "US", "USA"},
     {"12.169.0.0", "US", "USA"},
     {"12.200.0.0", "US", "USA"},
     {"203.121.0.8", "MY", "MYS"},
     {"203.20.231.1", "AU", "AUS"},
     {"203.87.98.29", "AU", "AUS"},
     {"203.181.121.150", "JP", "JPN"},
     {"202.166.127.246", "SG", "SGP"},
     {"62.188.202.242", "GB", "GBR"},
     {"12.12.197.23", "US", "USA"},
     {"12.12.199.3", "US", "USA"},
     {"12.12.200.79", "US", "USA"}].
