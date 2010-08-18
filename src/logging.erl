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
	    io:format("YYYYMMDD-HHMMSS - [~p] - ~p~n", [From, Message]),
	    loop();
	_Any ->
	    loop()
       end.
