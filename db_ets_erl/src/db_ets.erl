-module(db_ets).

-export([start_link/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([handle_telemetry_event/4]).

-behavior(gen_server).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init(_Args) ->
    start_prometheus(),
    declare_prometheus_metrics(),
    attach_telemetry(),
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
    telemetry:execute([db_ets, write, request], #{count => 1}, #{}),
    {noreply, LoopData};

handle_cast({delete, Key}, LoopData) ->
    ets:delete(db, Key),
    telemetry:execute([db_ets, db_elements, event], #{count => -1}, #{}),
    {noreply, LoopData};

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({read, Key}, _From, LoopData) ->
    SpanFun =
        fun() ->
            Reply = case ets:lookup(db, Key) of
                            [] -> {error, instance};
                            [{Key, Value}] -> {ok, Value}
                    end, {Reply, #{}}
        end,
    Reply = telemetry:span([db_ets, read], #{}, SpanFun),
    %Reply = case ets:lookup(db, Key) of
    %            [] -> {error, instance};
    %            [{Key, Value}] -> {ok, Value}
    %        end,
    {reply, Reply, LoopData};

handle_call({match, Element}, _From, LoopData) ->
    Reply = lists:flatten(ets:match(db, {'$1', Element})),
    {reply, Reply, LoopData}.

%% ===========================================================
%% Metrics
%% ===========================================================
start_prometheus() ->
    prometheus:start(),
    prometheus_httpd:start(),
    prometheus_http_impl:setup().

declare_prometheus_metrics() ->
    prometheus_counter:declare([{name, db_ets_write_count},
                                {labels, []},
                                {help, "Number of writes"}]),
    prometheus_gauge:declare([{name, db_ets_db_elements_gauge},
                              {labels, []},
                              {help, "Size of ets_db"}]),
    prometheus_histogram:declare([{name, db_ets_read_duration_microseconds},
                                  {buckets, [5, 10, 20, 40, 80]},
                                  {labels, []},
                                  {help, "Duration to read from db_ets"}]).

attach_telemetry() ->
    ok = telemetry:attach_many(
        <<"db_ets-handlers">>,
        [
         [db_ets, write, request],
         [db_ets, db_elements, event],
         [db_ets, read, start],
         [db_ets, read, stop],
         [db_ets, read, exception]
        ],
        fun ?MODULE:handle_telemetry_event/4,
        #{}
       ).

handle_telemetry_event([db_ets, write, request], #{count := Count}, #{}, _Config) ->
    prometheus_counter:inc(db_ets_write_count, [], Count),
    prometheus_gauge:inc(db_ets_db_elements_gauge, [], Count);
handle_telemetry_event([db_ets, db_elements, event], #{count := Count}, #{}, _Config) ->
    prometheus_gauge:dec(db_ets_db_elements_gauge, [], Count);
handle_telemetry_event([db_ets, read, stop], #{duration := Duration}, #{}, _Config) ->
    prometheus_histogram:observe(db_ets_read_duration_microseconds, [], Duration);
handle_telemetry_event(_, #{}, #{}, _Config) ->
    ok.