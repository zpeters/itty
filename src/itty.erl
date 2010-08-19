%%%-------------------------------------------------------------------
%%% File    : itty.erl
%%% Author  : Zach Peters <zach@thehelpfulhacker.net>
%%% Description : An itty bitty web server
%%%
%%% Created : 16 Aug 2010 by Zach Peters <zach@thehelpfulhacker.net>
%%%-------------------------------------------------------------------
-module(itty).
-export([start/0, stop/0, loop/0, serve/0]).


%%% Defines
-define(PORT, 5678).


%%%-------------------------------------------------------------------
%%% API
start() ->
    logging:log(?MODULE, "Starting..."),
    Pid = spawn(?MODULE, loop, []),
    register(itty, Pid),
    ok.

stop() ->
    logging:log(?MODULE, "Stopping..."),
    itty ! stop,
    unregister(itty),
    ok.


%%%-------------------------------------------------------------------
%%% Internal
loop() ->
    receive
	stop ->
	    ok;
	{get, Path} ->
	    logging:log(?MODULE, io_lib:format("GET '~s'", [Path])),
	    logging:log(?MODULE, "Spawing server"),
	    spawn(?MODULE, serve, []);
	Any ->
	    logging:log(?MODULE, io_lib:format("Got signal '~s'", [Any])),
	    loop()
    end.


serve() ->
    io:format("Attaching socket to port [~p]~n", [?PORT]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, 
					[binary, {packet, http},
					 {active, false}]),
    {ok, RecvSocket} = gen_tcp:accept(ListenSocket),
    {ok, {http_request, HttpMethod, HttpUri, HttpVersion}} = gen_tcp:recv(RecvSocket, 0),
    io:format("Processing request...~n"),
    io:format("HTTP Method: ~p~n", [HttpMethod]),
    io:format("HTTP URI: ~p~n", [HttpUri]),
    io:format("HTTP Version: ~p~n", [HttpVersion]),
    io:format("Sending Response...~n"),
    Body = io_lib:format("<html><head><title>hello</title></head><body><h1>Hello, world</h1><hr><b>~p</b></body></html>", [erlang:date()]),
    {Header, NewBody} = gen_header(200, Body),
    Packet = string:concat(Header,NewBody),
    io:format("Packet: ~p~n", [Packet]),
    ok = gen_tcp:send(RecvSocket, Packet),
    ok = gen_tcp:close(RecvSocket),
    ok = gen_tcp:close(ListenSocket),
    ok.

gen_header(Response, Body) ->
    Version = "HTTP/1.0",
    case Response of 
	200 ->
	    ResponseCode = "200 OK",
	    NewBody = Body;
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
    



			  
