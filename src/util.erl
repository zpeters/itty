-module(util).
-compile([export_all]).

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
