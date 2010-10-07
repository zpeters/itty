-module(config).
-compile([export_all]).

-define(DEFAULT_CONFIG, "application.cfg").
-define(SITE_CONFIG, "site.cfg").


start() ->
    io:format("Spawning config...~n"),
    Pid = spawn(config, loop, [[]]),
    register(configLoop, Pid),
    configLoop ! start.

stop() ->
    io:format("Stopping loop..~n"),
    configLoop ! stop,
    unregister(configLoop).

dump() ->
    configLoop ! dump.

get(Key) ->    
    configLoop ! {get, Key, self()},
    receive
	{error, E} ->
	    {error, E};
	Value ->
	    Value
    end.

loop(Config) ->
    receive
	stop ->
	    ok;
	start ->
	    {ok, DefaultConfig} = file:consult(?DEFAULT_CONFIG),
	    case test_local_config() of
		ok ->
		    {ok, SiteConfig} = file:consult(?SITE_CONFIG),
		    UpdatedConfig = update_config(DefaultConfig, SiteConfig);
		error ->
		    UpdatedConfig = DefaultConfig
	    end,
	    loop(UpdatedConfig);
	dump ->
	    io:format("~p~n", [Config]),
	    loop(Config);
	{get, Key, Pid} ->
	    Pid ! get_key(Key, Config),
	    loop(Config);
	Signal ->
	    io:format("I got signal: ~p~n", [Signal]),
	    loop(Config)
    end.


 get_key(_Key, []) ->
     {error, not_found};
 get_key(Key, [{Key, Value} | _Config]) ->
     Value;
 get_key(Key, [{_Other, _Value} | Config]) ->
     get_key(Key, Config).

update_config(Config, []) ->
    Config;
update_config(OldConfig, [{Key, Value} | NewConfig]) ->
    UpdatedConfig = lists:keystore(Key, 1, OldConfig, {Key, Value}),
    update_config(UpdatedConfig, NewConfig).

test_local_config() ->
    case file:read_file_info(?SITE_CONFIG) of
	{ok, _Info} ->
	    ok;
	_Any ->
	    error
    end.



   
