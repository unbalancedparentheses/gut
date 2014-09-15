-module(gutenberg).
-export([
         start/0,
         start/2,
         stop/0,
         stop/1
        ]).

start() ->
    {ok, _Started} = application:ensure_all_started(gutenberg).

stop() ->
    application:stop(gutenberg).

start(_StartType, _StartArgs) ->
    gute_sup:start_link().

stop(_State) ->
    ok.
