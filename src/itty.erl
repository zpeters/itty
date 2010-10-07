-module(itty).
-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defines

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
    config:start(),
    case config:get(debug) of
	true ->
	    config:dump()
    end,
    BootString = io_lib:format("Itty ~p starting on port: ~p",
			       [config:get(version),
				config:get(port)]),
    log_event(BootString),
    do_listen(config:get(port), config:get(tcp_options), handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Control Functions

do_listen(Port, Opts, Handler) ->
    case gen_tcp:listen(Port, Opts) of
	{ ok, ListeningSocket } ->
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    handle_error(E)
    end.

listen_loop(ListeningSocket, Handler) ->
    case gen_tcp:accept(ListeningSocket) of
	{ ok, ConnectedSocket } ->
	    spawn(node(), ?MODULE, Handler, [ConnectedSocket]),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    handle_error(E)
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
	{ok, {200, Body, MimeType}} ->
	    Packet = gen_packet(200, Body, MimeType);
	{error, {404, not_found}} ->
	    Packet = gen_packet({error, {404, not_found}}, config:get(body_404));
	{ error, _Any } ->
	    Packet = gen_packet({error, {500, not_found}}, config:get(body_500))
    end,
    gen_tcp:send(ConnectedSocket, Packet),
    gen_tcp:close(ConnectedSocket).

serve_request(RequestRecord) ->
    Request = config:get(docroot) ++ get_index(RequestRecord#http_request.http_path),
    case file:read_file(Request) of
	{ok, File} ->
	    FileContents = binary_to_list(File),
	    BodyLength = string:len(FileContents),
	    MimeType = mime:guess_mime(Request),
	    log_request(RequestRecord, 200, BodyLength),
	    {ok, {200, FileContents, MimeType}};
	_ ->
	    BodyLength = string:len(config:get(body_404)),
	    log_request(RequestRecord, 404, BodyLength),
	    {error, {404, not_found}}
    end.

	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HTTP Utility Functions

% tests Path to see if we are requesting a resource
% if not we will request with DIRECTORYINDEX appended to the end
get_index(Path) ->
    Len = string:len(Path),
    LastChar = lists:sublist(Path,Len,Len),
    case LastChar of 
	"/" ->
	    Index = atom_to_list(config:get(directory_index)),
	    NewPath = Path ++ Index;
	_Any ->
	    NewPath = Path
    end,
    NewPath.


gen_header({error, ResponseCode}, MimeType) ->
    case ResponseCode of
	{404, not_found} ->
	    gen_header(404, config:get(body_404), MimeType);
	_Other ->
	    gen_header(500, config:get(body_500), MimeType)
    end.
gen_header(ResponseCode, Body, MimeType) ->
    case ResponseCode of
	200 ->
	    ResponseCodeString = "200 OK";
	404 ->
	    ResponseCodeString = "404 Not Found";
	_Any ->
	    ResponseCodeString = "500 FFFFFFFUUUUUUUUUUUU"
    end,
    Version = "HTTP/1.0",
    HttpResponse = io_lib:format("~s ~s\r\n", [Version, ResponseCodeString]),
    Server = "Server: itty/0.1\r\n",
    ContentType = io_lib:format("Content-Type: ~s\r\n", [MimeType]),
    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
    Header = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentLength, ContentType]),
    Header.

gen_packet(Response, Body) ->
    Header = gen_header(Response, Body),
    string:concat(Header, Body).
gen_packet(Response, Body, MimeType) ->
    Header = gen_header(Response, Body, MimeType),
    string:concat(Header, Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility Functions
log_request(RequestRecord, StatusCode, BodyLength) ->
    LogFile = config:get(http_logfile),
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    LogString = io_lib:format("~s ~p ~p ~s [~p-~p-~p ~p:~p:~p ~s] \"~p ~p HTTP/~p.~p\" ~p ~p", 
			      [
			       ip_tuple_to_string(RequestRecord#http_request.requestor_ip),
			       RequestRecord#http_request.client_identity,
			       RequestRecord#http_request.client_username,
			       ip_tuple_to_string(RequestRecord#http_request.requestor_ip),
			       Year, Month, Day,
			       Hour, Minute, Second,
			       time_zone(),
			       RequestRecord#http_request.http_method,
			       RequestRecord#http_request.http_path,
			       RequestRecord#http_request.http_ver_maj,
			       RequestRecord#http_request.http_ver_min,
			       StatusCode, BodyLength]),
    log(LogString, LogFile).

log_event(String) ->
    LogFile = config:get(event_logfile),
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    LogString = io_lib:format("~p~p~p ~p:~p:~p - ~s",
			      [
			       Year, Month, Day,
			       Hour, Minute, Second,
			       String]),
    log(LogString, LogFile).
			       
log(String, File) ->
    {ok, LogFile} = file:open(File, [append]),
    io:fwrite(LogFile, "~s~n", [String]),
    file:close(LogFile).

gen_time() ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = erlang:localtime(),
    Date = io_lib:format("~p~p~p ~p:~p:~p~n", [Year, Month, Day, Hour, Min, Seconds]),
    Date.

ip_tuple_to_string(IpTuple) ->
    {Octet1, Octet2, Octet3, Octet4} = IpTuple,
    IpAddress = io_lib:format("~p.~p.~p.~p", [Octet1, Octet2, Octet3, Octet4]),
    IpAddress.

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

handle_error(Error) ->
    io:format("Got error: ~p~n", [Error]).
