-module(gutenberg).
-export([
         main/1,
         start/0,
         start/2,
         stop/0,
         stop/1
        ]).

main(_) ->
    io:format("Hola").

start() ->
    {ok, _Started} = application:ensure_all_started(gutenberg).

stop() ->
    application:stop(gutenberg).

start(_StartType, _StartArgs) ->
    gute_sup:start_link().

stop(_State) ->
    ok.
