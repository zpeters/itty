-module(itty_config).
-compile([export_all]).

-define(CONFIG_FILE, "/home/zach/Projects/itty/application.cfg").

start() ->
    {ok, Config} = file:consult(?CONFIG_FILE),
    Config.
	  
get(_Key, []) ->
    {error, not_found};
get(Key, [{Key, Value} | _Config]) ->
    Value;
get(Key, [{_Other, _Value} | Config]) ->
    get(Key, Config).
	    




   
