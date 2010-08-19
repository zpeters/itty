-module(boot).

-export([start/0]).

start() ->
    io:format("Booting...~n"),
    logging:start(),
    itty:start(),
    itty ! hello,
    io:format("Stopping...~n"),
    itty:stop(),
    logging:stop(),
    ok.
