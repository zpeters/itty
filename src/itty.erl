-module(itty).
-export([start/0, handler/1, gen_time/0]).

-define(TCP_OPTS, [list,
		   {active, false},
		   {packet, http}]).
-define(PORT, 56789).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
start() ->
    io:format("Initializing server...~n"),
    do_listen(?PORT, ?TCP_OPTS, handler).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Control Functions

do_listen(Port, Opts, Handler) ->
    case gen_tcp:listen(Port, Opts) of
	{ ok, ListeningSocket } ->
	    io:format("Server bound to port ~p~n", [Port]),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    { error, E }
    end.

listen_loop(ListeningSocket, Handler) ->
    io:format("\tMonitoring listening socket for connections...~n"),
    case gen_tcp:accept(ListeningSocket) of
	{ ok, ConnectedSocket } ->
	    io:format("\t\tGot a connection, spawning handler~n"),
	    spawn(node(), ?MODULE, Handler, [ConnectedSocket]),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    { error, E }
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler   
handler(ConnectedSocket) ->
    Ref = make_ref(),
    io:format("\t\tHandler [~p] at your service...~n", [Ref]),
    {ok, {http_request, HttpMethod, HttpUri, HttpVersion}} = gen_tcp:recv(ConnectedSocket, 0),
    io:format("\t\t\tHttpMethod: ~p~n", [HttpMethod]),
    io:format("\t\t\tHttpUri: ~p~n", [HttpUri]),
    io:format("\t\t\tHttpVersion: ~p~n", [HttpVersion]),
    Body = "<html><head><title>Hello, world</title><body><h1>" ++ gen_time() ++ "</h1></body></html>",
    ResponseCode = "200 OK",
    Version = "HTTP/1.0",
    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCode]),
    Server = "Server: itty/0.1\r\n",
    ContentType = "Content-Type: text/html\r\n",
    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
    Header = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentType, ContentLength]),
    Packet = string:concat(Header, Body),
    io:format("\t\t\tHandler ~p sending packet~n", [Ref]),
    gen_tcp:send(ConnectedSocket, Packet),
    gen_tcp:close(ConnectedSocket).
	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility Functions
gen_time() ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = erlang:localtime(),
    Date = io_lib:format("~p~p~p ~p:~p:~p~n", [Year, Month, Day, Hour, Min, Seconds]),
    Date.
    
