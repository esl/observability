-module(db_ets).
-export([start_link/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args) ->
    {ok, ets:new(db, [named_table, set])}.

terminate(_Reason, _LoopData) ->
    ets:delete(db).

write(Key, Element) ->
    gen_server:cast(?MODULE, {write, Key, Element}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

read(Key) ->
    gen_server:call(?MODULE, {read, Key}).

match(Element) ->
    gen_server:call(?MODULE, {match, Element}).

handle_cast({write, Key, Element}, LoopData) ->
    ets:insert(db, {Key, Element}),
    {noreply, LoopData};

handle_cast({delete, Key}, LoopData) ->
    ets:delete(db, Key),
    {noreply, LoopData};

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({read, Key}, _From, LoopData) ->
    Reply = case ets:lookup(db, Key) of
		[] -> {error, instance};
		[{Key, Value}] -> {ok, Value}
	    end,
    {reply, Reply, LoopData};

handle_call({match, Element}, _From, LoopData) ->
    Reply = lists:flatten(ets:match(db, {'$1', Element})),
    {reply, Reply, LoopData}.




