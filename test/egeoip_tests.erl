-module(egeoip_tests).
-include_lib("eunit/include/eunit.hrl").
-include("egeoip.hrl").

run_test_() ->
    {inorder,
     {foreach,
      fun egeoip:start/0,
      fun(_) -> egeoip:stop() end,
      [{"egeoip_bench", fun egeoip_bench/0},
       {"egeoip", fun egeoip/0},
       {"egeoip_lookup", fun egeoip_lookup/0},
       {"egeoip_reserved", {generator, fun egeoip_reserved_/0}},
       {"non_parallel", fun non_parallel/0}
      ]
     }}.

egeoip_reserved_() ->
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
