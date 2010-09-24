-module(itty_config).
-compile([export_all]).

-define(CONFIG_FILE, "/home/zach/Projects/itty/application.cfg").

start() ->
    Pid = spawn(node(), ?MODULE, loop, []),
    register(config, Pid).

stop() ->
    config ! stop.

loop(Config) ->
    receive
	start ->
	    {ok, Config} = file:consult(?CONFIG_FILE),
	    loop(Config);
	stop ->
	    ok;
	status ->
	    io:format("Config: ~p~n", [Config]),
	    loop(Config);
	Any ->
	    io:format("I got signal: ~p~n", [Any]),
	    loop(config)
    end.
    
	    




   
