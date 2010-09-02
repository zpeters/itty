-module(itty).
-export([start/0, handler/1, gen_time/0, time_zone/0, time_zone/1]).

-define(TCP_OPTS, [list,
		   {active, false},
		   {packet, http}]).
-define(PORT, 8000).
-define(DOCROOT, "/home/zach/tmp").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
start() ->
    io:format("Initializing server...~n"),
    io:format("Port: ~p~n", [?PORT]),
    io:format("TCP Options: ~p~n", [?TCP_OPTS]),
    do_listen(?PORT, ?TCP_OPTS, handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Control Functions

do_listen(Port, Opts, Handler) ->
    case gen_tcp:listen(Port, Opts) of
	{ ok, ListeningSocket } ->
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    io:format("Error: ~p~n", [E]),
	    { error, E }
    end.

listen_loop(ListeningSocket, Handler) ->
    case gen_tcp:accept(ListeningSocket) of
	{ ok, ConnectedSocket } ->
	    spawn(node(), ?MODULE, Handler, [ConnectedSocket]),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    io:format("Error: ~p~n", [E]),
	    { error, E }
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler   
handler(ConnectedSocket) ->
    case gen_tcp:recv(ConnectedSocket, 0) of
	{ok, {http_request, HttpMethod, HttpUri, HttpVersion}} ->
	    {_HttpReq, HttpPath} = HttpUri,
	    {ok, {RequestorIP, _ClientPort}} = inet:peername(ConnectedSocket),
	    case serve_request(RequestorIP, HttpPath, HttpMethod, HttpVersion) of
		{ok, {200, Body}} ->
		    ResponseCode = "200 OK",
		    Version = "HTTP/1.0",
		    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCode]),
		    Server = "Server: itty/0.1\r\n",
		    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
		    Header = io_lib:format("~s~s~s\r\n", [HttpResponse, Server, ContentLength]),
		    Packet = string:concat(Header, Body);
		{error, {404, not_found}} ->
		    Body = "<html><head><title>404 Not Found</title></head><body>404 - LOL</body></html>",
		    ResponseCode = "404 Not Found",
		    Version = "HTTP/1.0",
		    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCode]),
		    Server = "Server: itty/0.1\r\n",
		    ContentType = "Content-Type: text/html\r\n",
		    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
		    Header = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentType, ContentLength]),
		    Packet = string:concat(Header, Body);
		{error, Error} ->
		    Body = "<html><head><title>500 FUUUUuuuu</title></head><body>500 - FUUUUuuuu<br>" ++ Error ++ "</body></html>",
		    ResponseCode = "500 FUUUUuuuu",
		    Version = "HTTP/1.0",
		    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCode]),
		    Server = "Server: itty/0.1\r\n",
		    ContentType = "Content-Type: text/html\r\n",
		    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
		    Header = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentType, ContentLength]),
		    Packet = string:concat(Header, Body)
	    end,
	    gen_tcp:send(ConnectedSocket, Packet),
	    gen_tcp:close(ConnectedSocket);
	Any ->
	    io:format("I got request: ~p~n", [Any])
    end.

serve_request(RequestorIP, HttpPath, HttpMethod, HttpVersion) ->
    case HttpPath of 
	[] ->
	    Request = ?DOCROOT ++ "/index.html";
	"/" ->
	    Request = ?DOCROOT ++ "/index.html";
	_Any ->
	    Request = ?DOCROOT ++ HttpPath
    end,
    case file:read_file(Request) of
	{ok, File} ->
	    FileContents = binary_to_list(File),
	    ClientIdentity = "-",
	    ClientUsername = "-",
	    {Year, Month, Day} = date(),
	    {Hour, Minute, Second} = time(),
	    TimeZone = time_zone(),
	    {MajorVersion, MinorVersion} = HttpVersion,
	    BodyLength = string:len(FileContents),
	    io:format("~p ~s ~s [~p-~p-~p ~p:~p:~p ~s] \"~s ~s HTTP/~p.~p\" ~p ~p~n", 
		      [RequestorIP, ClientIdentity, ClientUsername,
		       Year, Month, Day,
		       Hour, Minute, Second, 
		       TimeZone,
		      HttpMethod, HttpPath,
		      MajorVersion, MinorVersion,
		      200, BodyLength]),
	    {ok, {200, FileContents}};
	_ ->
	    {error, {404, not_found}}
    end.

	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility Functions
gen_time() ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = erlang:localtime(),
    Date = io_lib:format("~p~p~p ~p:~p:~p~n", [Year, Month, Day, Hour, Min, Seconds]),
    Date.

%% Seen here - http://www.erlang.org/pipermail/erlang-questions/2006-December/024289.html
time_zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
	calendar:datetime_to_gregorian_seconds(Time),
    time_zone((DiffSecs/3600)*100).
time_zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
time_zone(Val) when Val >= 0 ->		           
    io_lib:format("+~4..0w", [trunc(abs(Val))]).
