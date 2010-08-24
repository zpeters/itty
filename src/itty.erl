-module(itty).
-export([start/0]).
-define(PORT, 5678).

start() ->
    io:format("Initializing server on port ~p...~n", [?PORT]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT,
					[list,
					 {active, false},
					 {keepalive, true},
					 {packet, http},
					 {reuseaddr, true}]),
    server(ListenSocket).

server(ListenSocket) ->
    handle(ListenSocket),
    server(ListenSocket).
   
handle(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, {http_request, HttpMethod, HttpUri, _HttpVersion}} = gen_tcp:recv(Socket, 0),
    io:format("Processing request...~n"),
    io:format("HTTP Method: ~p~n", [HttpMethod]),
    io:format("HTTP URI: ~p~n", [HttpUri]),
    Body = "<html><head><title>Hello</title></head><body><h1>Hello, world</h1></body></html>",
    {Header, NewBody} = gen_header(200, Body),
    Packet = string:concat(Header, NewBody),
    ok = gen_tcp:send(Socket, Packet),
    ok = gen_tcp:close(Socket).

gen_header(Response, Body) ->
    Version = "HTTP/1.0",
    case Response of
        200 ->
            ResponseCode = "200 OK",
	    NewBody = Body ++ "<hr>" ++ gen_time();
        404 ->
            ResponseCode = "404 Not Found",
            NewBody = "<h1>Not Found</h1>";
        _Any ->
            ResponseCode = "500 Intern Server Error FUUUUUuuuu",
            NewBody = "<h1>FFFFUUUUuuuuu</h1>"
    end,
    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCode]),
    Server = "Server: itty/0.1\r\n",
    ContentType = "Content-Type: text/html\r\n",
    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(NewBody)]),
    HttpHeader = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentType, ContentLength]),
    {HttpHeader, NewBody}.

gen_time() ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = erlang:localtime(),
    Date = io_lib:format("~p~p~p ~p:~p:~p~n", [Year, Month, Day, Hour, Min, Seconds]),
    Date.
    
