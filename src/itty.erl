-module(itty).
-compile([export_all]).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
start() ->
    config:start(),
    BootString = io_lib:format("Itty ~p starting on port: ~p~n",
			       [config:get(version),
				config:get(port)]),
    log:log_event(BootString),
    io:format(BootString),
    case config:get(debug) of
	true ->
	    config:dump();
	_Any ->
	    ok
    end,
    template:start(),
    do_listen(config:get(port), config:get(tcp_options), handler).

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
	{ok, {200, Body, MimeType}} ->
	    Packet = gen_packet(200, Body, MimeType);
	{error, {404, not_found}} ->
	    Packet = gen_packet({error, {404, not_found}}, template:get(404));
	{ error, _Any } ->
	    Packet = gen_packet({error, {500, not_found}}, template:get(500))
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
	    log:log_request(RequestRecord, 200, BodyLength),
	    {ok, {200, FileContents, MimeType}};
	_ ->
	    BodyLength = string:len(template:get(404)),
	    log:log_request(RequestRecord, 404, BodyLength),
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
	    gen_header(404, template:get(404), MimeType);
	_Other ->
	    gen_header(500, template:get(500), MimeType)
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
