-module(log).
-compile([export_all]).
-include("records.hrl").

log_request(RequestRecord, StatusCode, BodyLength) ->
    LogFile = config:get(http_logfile),
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    LogString = io_lib:format("~s ~p ~p ~s [~p-~p-~p ~p:~p:~p ~s] \"~p ~p HTTP/~p.~p\" ~p ~p", 
			      [
			       util:ip_tuple_to_string(RequestRecord#http_request.requestor_ip),
			       RequestRecord#http_request.client_identity,
			       RequestRecord#http_request.client_username,
			       util:ip_tuple_to_string(RequestRecord#http_request.requestor_ip),
			       Year, Month, Day,
			       Hour, Minute, Second,
			       util:time_zone(),
			       RequestRecord#http_request.http_method,
			       RequestRecord#http_request.http_path,
			       RequestRecord#http_request.http_ver_maj,
			       RequestRecord#http_request.http_ver_min,
			       StatusCode, BodyLength]),
    log:log(LogString, LogFile).

log_event(String) ->
    LogFile = config:get(event_logfile),
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    LogString = io_lib:format("~p~p~p ~p:~p:~p - ~s",
			      [
			       Year, Month, Day,
			       Hour, Minute, Second,
			       String]),
    log:log(LogString, LogFile).
			       
log(String, File) ->
    {ok, LogFile} = file:open(File, [append]),
    io:fwrite(LogFile, "~s~n", [String]),
    file:close(LogFile).
