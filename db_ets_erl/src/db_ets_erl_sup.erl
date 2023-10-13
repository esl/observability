%%%-------------------------------------------------------------------
%% @doc db_ets_erl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(db_ets_erl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => db_ets,
                    start => {db_ets, start_link, []},
                    type => worker}],
    {ok, {SupFlags, ChildSpecs}}.
