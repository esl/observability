%%%-------------------------------------------------------------------
%% @doc db_ets_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(db_ets_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    db_ets_erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
