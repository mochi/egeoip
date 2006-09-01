%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

-module(egeoip_sup).
-author('bob@redivi.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    File = case application:get_env(egeoip, dbfile) of
	       {ok, Other} ->
		   Other;
	       _ ->
		   city
	   end,
    Process = {egeoip,
	       {egeoip, start, [File]}, permanent, 5000, worker, [egeoip]},
    {ok, {{one_for_one, 0, 300}, [Process]}}.	   
    
