-module(template).
-compile([export_all]).

start() ->
    Pid = spawn(template, loop, []),
    register(templateLoop, Pid).

stop() ->
    templateLoop ! stop,
    unregister(templateLoop).

get(Template) ->    
    templateLoop ! {get, Template, self()},
    receive
	{error, _ErrorInfo} ->
	    template:get(500);
	Value ->
	    Value
    end.

loop() ->
    receive
	stop ->
	    ok;
	{get, Code, Pid} ->	    
	    Pid ! get_template(Code),
	    loop()
    end.

get_template(Template) ->
    Filename = io_lib:format("html/~p.html", [Template]),
    case file:read_file(Filename) of
	{ok, Binary} ->
	    binary_to_list(Binary);
	{error, E} ->
	    {error, E}
    end.

