-module(itty).
-export([start/0, handler/1, 
	 gen_header/1, gen_header/2, gen_time/0, 
	 time_zone/0, time_zone/1,
	gen_http_request_record/2]).

-define(TCP_OPTS, [list,
		   {active, false},
		   {packet, http}]).
-define(PORT, 8000).
-define(DOCROOT, "/home/zach/tmp").

-define(BODY_404, "<html><head><title>404 Not Found</title></head><body>404 - LOL</body></html>").
-define(BODY_500, "<html><head><title>500 FUUUUuuuu</title></head><body>500 - FUUUUuuuu<br>Unknown error</body></html>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% records
-record(http_request,
	{requestor_ip,
	 http_path,
	 http_method,
	 http_ver_maj,
	 http_ver_min,
	 client_identity="-",
	 client_username="-"
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
start() ->
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
	    { error, E }
    end.

listen_loop(ListeningSocket, Handler) ->
    case gen_tcp:accept(ListeningSocket) of
	{ ok, ConnectedSocket } ->
	    spawn(node(), ?MODULE, Handler, [ConnectedSocket]),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    { error, E }
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handler   

gen_http_request_record(HttpRequest, RequestorIP) ->
    {http_request, HttpMethod, HttpUri, HttpVersion} = HttpRequest,
    {MajVer, MinVer} = HttpVersion,
    {_HttpReq, HttpPath} = HttpUri,
    RequestRecord = #http_request{
      requestor_ip=RequestorIP,
      http_path=HttpPath,
      http_method=HttpMethod,
      http_ver_maj=MajVer,
      http_ver_min=MinVer
     },
    RequestRecord.

handler(ConnectedSocket) ->
    {ok, HttpRequest} = gen_tcp:recv(ConnectedSocket, 0),
    {ok, {RequestorIP, _ClientPort}} = inet:peername(ConnectedSocket),
    RequestRecord = gen_http_request_record(HttpRequest, RequestorIP),
    case serve_request(RequestRecord) of
	{ok, {200, Body}} ->
	    Packet = gen_packet(200, Body);
	{error, {404, not_found}} ->
	    Packet = gen_packet({error, {404, not_found}}, ?BODY_404);
	{ error, _Any } ->
	    Packet = gen_packet({error, {500, not_found}}, ?BODY_500)
    end,
    gen_tcp:send(ConnectedSocket, Packet),
    gen_tcp:close(ConnectedSocket).

serve_request(RequestRecord) ->
    case RequestRecord#http_request.http_path of 
	[] ->
	    Request = ?DOCROOT ++ "/index.html";
	"/" ->
	    Request = ?DOCROOT ++ "/index.html";
	_Any ->
	    Request = ?DOCROOT ++ RequestRecord#http_request.http_path
    end,
    case file:read_file(Request) of
	{ok, File} ->
	    FileContents = binary_to_list(File),
	    BodyLength = string:len(FileContents),
	    log_request(RequestRecord, 200, BodyLength),
	    {ok, {200, FileContents}};
	_ ->
	    BodyLength = string:len(?BODY_404),
	    log_request(RequestRecord, 404, BodyLength),
	    {error, {404, not_found}}
    end.

	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HTTP Utility Functions
gen_header({error, {500, Error}}) ->
    Body = "<html><head><title>500 FUUUUuuuu</title></head><body>500 - FUUUUuuuu<br>" ++ Error ++ "</body></html>",
    gen_header(500, Body);
gen_header({error, ResponseCode}) ->
    case ResponseCode of
	{404, not_found} ->
	    gen_header(404, ?BODY_404);
	_Other ->
	    gen_header(500, ?BODY_500)
    end.
gen_header(ResponseCode, Body) ->
    case ResponseCode of
	200 ->
	    ResponseCodeString = "200 OK";
	404 ->
	    ResponseCodeString = "404 Not Found";
	_Any ->
	    ResponseCodeString = "500 FUUUuuu"
    end,
    Version = "HTTP/1.0",
    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCodeString]),
    Server = "Server: itty/0.1\r\n",
    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
    Header = io_lib:format("~s~s~s\r\n", [HttpResponse, Server, ContentLength]),
    Header.

gen_packet(Response, Body) ->
    Header = gen_header(Response, Body),
    string:concat(Header, Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility Functions
log_request(RequestRecord, StatusCode, BodyLength) ->
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    io:format("~p ~p ~p ~p [~p-~p-~p ~p:~p:~p ~s] \"~p ~p HTTP/~p.~p\" ~p ~p~n", 
	      [
	       RequestRecord#http_request.requestor_ip,
	       RequestRecord#http_request.client_identity,
	       RequestRecord#http_request.client_username,
	       RequestRecord#http_request.requestor_ip,
	       Year, Month, Day,
	       Hour, Minute, Second,
	       time_zone(),
	       RequestRecord#http_request.http_method,
	       RequestRecord#http_request.http_path,
	       RequestRecord#http_request.http_ver_maj,
	       RequestRecord#http_request.http_ver_min,
	       StatusCode, BodyLength]).

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
