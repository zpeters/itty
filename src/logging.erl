-module(logging).
-export([start/0, stop/0, loop/0, log/2]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    register(logger, Pid).

stop() ->
    logger ! stop,
    unregister(logger),
    ok.

log(From, Message) ->
    logger ! {message, {From, Message}}.

loop() ->
    receive
	stop ->
	       io:format("Loop got 'stop' signal~n"),
	       ok;
	{message, {From, Message}} ->
	    {{Year,Month,Day},{Hour,Min,Seconds}} = erlang:localtime(),
	    io:format("~p~p~p-~p:~p:~p - [~s] - ~s~n", [Year, Month, Day, Hour, Min, Seconds, From, Message]),
	    loop();
	_Any ->
	    loop()
       end.
