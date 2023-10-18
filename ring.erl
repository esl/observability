-module(ring).
-compile(export_all).



start_ring(N,Mgs) ->
    Pid = start(N, self()),
    main_loop(Pid, Mgs),
    receive stop -> stop end,
    seq_trace:print("Stopping:~p~n",[self()]).

main_loop(Pid, 0) ->
    Pid ! stop;
main_loop(Pid, Mgs) ->
    Pid ! hello,
    seq_trace:print("~p sent msg to ~p~n",[self(), Pid]),
    receive hello -> ok end,
    main_loop(Pid, Mgs-1).

start(1, Pid) ->
    seq_trace:print("~p connected to ~p~n",[self(), Pid]),
    Pid; 
start(N, Pid) ->
    NewPid = spawn(ring, loop, [Pid]),
    start(N-1, NewPid).

loop(Pid) ->
    seq_trace:print("~p connected to ~p~n",[self(), Pid]),
    receive 
	hello -> 
	    Pid ! hello,
	    seq_trace:print("~p sent msg to ~p~n",[self(), Pid]),
	    loop(Pid);
	stop -> 
	    Pid ! stop,
	    seq_trace:print("~p stopping~n",[self()])
    end.
    
    

