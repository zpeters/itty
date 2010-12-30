-module(itty).
-compile([export_all]).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
start() ->
    config:start(),
    log:debug("Config loaded..."),
    BootString = io_lib:format("Itty ~p booting",
			       [config:get(version)]),
    log:log_event(BootString),
    log:debug("Checking config"),
    case util:dirExists(config:get(docroot)) of 
	true ->
	    DirExistsString = io_lib:format("Docroot '~s' exists", [config:get(docroot)]),
	    log:debug(DirExistsString),
	    ok;
	false ->
	    DirExistsString = io_lib:format("Docroot '~s' missing", [config:get(docroot)]),
	    log:debug(DirExistsString),
	    erlang:error(missing_doc_root)
    end,
    log:debug("Starting Template"),
    template:start(),
    ListenerString = io_lib:format("Starting listener on port: ~p - with options: ~p", [config:get(port), config:get(tcp_options)]),
    log:debug(ListenerString),
    do_listen(config:get(port), config:get(tcp_options), handler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Control Functions

do_listen(Port, Opts, Handler) ->
    log:debug("Entering do_listen"),
    case gen_tcp:listen(Port, Opts) of
	{ ok, ListeningSocket } ->
	    ListeningSocketString = io_lib:format("Got ListeningSocket on port ~p", [Port]),
	    log:debug(ListeningSocketString),
	    listen_loop(ListeningSocket, Handler);
	{ error, E } ->
	    ErrorString = io_lib:format("Error '~s' getting ListeningSocket", [E]),
	    log:debug(ErrorString),
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
    log:debug("Entering handler"),
    {ok, HttpRequest} = gen_tcp:recv(ConnectedSocket, 0),
    {ok, {RequestorIP, _ClientPort}} = inet:peername(ConnectedSocket),
    RequestRecord = gen_http_request_record(HttpRequest, RequestorIP),
    RequestString = io_lib:format("Got request ~p from ~p", [HttpRequest, RequestorIP]),
    log:debug(RequestString),
    case serve_request(RequestRecord) of
	{ok, {200, Body, MimeType}} ->
	    log:debug("Generating packet for 200"),
	    Packet = gen_packet(200, Body, MimeType);
	{error, {404, not_found}} ->
	    log:debug("Generating packet for 404"),
	    Packet = gen_packet({error, {404, not_found}}, template:get(404));
	{ error, _Any } ->
	    log:debug("Generating packet for 500"),
	    Packet = gen_packet({error, {500, not_found}}, template:get(500))
    end,
    log:debug("Sending packet to ConnectedSocket"),
    gen_tcp:send(ConnectedSocket, Packet),
    log:debug("Closing ConnectdSocket"),
    gen_tcp:close(ConnectedSocket).

serve_request(RequestRecord) ->
    log:debug("Serving request"),
    Request = config:get(docroot) ++ get_index(RequestRecord#http_request.http_path),
    case file:read_file(Request) of
	{ok, File} ->
	    log:debug("Reading file"),
	    FileContents = binary_to_list(File),
	    BodyLength = string:len(FileContents),
	    MimeType = mime:guess_mime(Request),
	    log:log_request(RequestRecord, 200, BodyLength),
	    {ok, {200, FileContents, MimeType}};
	_ ->
	    log:debug("Generating 404 from template"),
	    BodyLength = string:len(template:get(404)),
	    log:log_request(RequestRecord, 404, BodyLength),
	    {error, {404, not_found}}
    end.

	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HTTP Utility Functions

% tests Path to see if we are requesting a resource
% if not we will request with DIRECTORYINDEX appended to the end
get_index(Path) ->
    log:debug("Getting index"),
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
    log:debug("Generating header"),
    case ResponseCode of
	{404, not_found} ->
	    gen_header(404, template:get(404), MimeType);
	_Other ->
	    gen_header(500, template:get(500), MimeType)
    end.
gen_header(ResponseCode, Body, MimeType) ->
    log:debug("Generating header"),
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
    Server = io_lib:format("Server: itty/~s\r\n",[config:get(version)]),
    ContentType = io_lib:format("Content-Type: ~s\r\n", [MimeType]),
    ContentLength = io_lib:format("Content-Length: ~p\r\n", [string:len(Body)]),
    Header = io_lib:format("~s~s~s~s\r\n", [HttpResponse, Server, ContentLength, ContentType]),
    Header.

gen_packet(Response, Body) ->
    log:debug("Generating packet"),
    Header = gen_header(Response, Body),
    string:concat(Header, Body).
gen_packet(Response, Body, MimeType) ->
    log:debug("Generating packet"),
    Header = gen_header(Response, Body, MimeType),
    string:concat(Header, Body).
