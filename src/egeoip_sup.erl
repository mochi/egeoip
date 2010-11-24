%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

-module(egeoip_sup).
-author('bob@redivi.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([worker_names/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    File = case application:get_env(egeoip, dbfile) of
	       {ok, Other} ->
		   Other;
	       _ ->
		   city
	   end,
    Processes = worker(tuple_to_list(worker_names()), File),
    {ok, {{one_for_one, 0, 300}, Processes}}.

worker_names() ->
    {egeoip_0,
     egeoip_1,
     egeoip_2,
     egeoip_3,
     egeoip_4,
     egeoip_5,
     egeoip_6,
     egeoip_7}.

worker([], _File) ->
    [];
worker([Name | T], File) ->
    [{Name,
     {egeoip, start_link, [Name, File]},
     permanent, 5000, worker, [egeoip]} | worker(T, File)].
