-module(itty).
-export([start/0, handler/1, gen_time/0]).

-define(TCP_OPTS, [list,
		   {active, false},
		   {packet, http}]).
-define(PORT, 8080).
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
    {ok, {http_request, _HttpMethod, HttpUri, _HttpVersion}} = gen_tcp:recv(ConnectedSocket, 0),
    {_Req, Path} = HttpUri,
    case serve_request(Path) of
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
    gen_tcp:close(ConnectedSocket).
	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility Functions
gen_time() ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = erlang:localtime(),
    Date = io_lib:format("~p~p~p ~p:~p:~p~n", [Year, Month, Day, Hour, Min, Seconds]),
    Date.
    
serve_request(UriRequest) ->
    case UriRequest of 
	[] ->
	    Request = ?DOCROOT ++ "/index.html";
	"/" ->
	    Request = ?DOCROOT ++ "/index.html";
	_Any ->
	    Request = ?DOCROOT ++ UriRequest
    end,
    io:format("Request: ~p~n", [Request]),
    case file:read_file(Request) of
	{ok, File} ->
	    FileContents = binary_to_list(File),
	    {ok, {200, FileContents}};
	_ ->
	    {error, {404, not_found}}
    end.
