%%%-------------------------------------------------------------------
%%% File    : itty.erl
%%% Author  : Zach Peters <zach@thehelpfulhacker.net>
%%% Description : An itty bitty web server
%%%
%%% Created : 16 Aug 2010 by Zach Peters <zach@thehelpfulhacker.net>
%%%-------------------------------------------------------------------
-module(itty).
-compile([export_all]).


%%% Defines
-define(PORT, 5678).


%%%-------------------------------------------------------------------
%%% Server
server() ->
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
    Body = "<html><head><title>hello</title></head><body><h1>Hello, world</h1></body></html>",
    %HttpResponse = "HTTP/1.0 200 OK\r\nDate: Sat, 15 Jan 2000 14:27:12 GMT\r\nServer: itty/0.1\r\nContent-Type: text/HTML\r\nContent-Length: 666\r\n\r\n",
    %HttpResponse = "HTTP/1.0 200 OK\r\nDate: Sat, 15 Jan 2000 14:27:12 GMT\r\nServer: itty/0.1\r\nContent-Type: text/HTML\r\n\r\n",
    {Header, NewBody} = gen_header(500, Body),
    Packet = string:concat(Header,NewBody),
    io:format("Packet: ~p~n", [Packet]),
    ok = gen_tcp:send(RecvSocket, Packet),
    ok = gen_tcp:close(RecvSocket),
    ok = gen_tcp:close(ListenSocket).

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
    



			  
