-module(config).
-compile([export_all]).

-define(DEFAULT_CONFIG, "application.cfg").
-define(SITE_CONFIG, "site.cfg").

start() ->
    {ok, DefaultConfig} = file:consult(?DEFAULT_CONFIG),
    % if there is a local config file load it
    case test_local_config() of
	ok ->
	    {ok, SiteConfig} = file:consult(?SITE_CONFIG),
	    UpdatedConfig = update_config(DefaultConfig, SiteConfig),
	    UpdatedConfig;
	error ->
	    DefaultConfig
    end.

 get(_Key, []) ->
     {error, not_found};
 get(Key, [{Key, Value} | _Config]) ->
     Value;
 get(Key, [{_Other, _Value} | Config]) ->
     get(Key, Config).

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



   
