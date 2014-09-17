-module(gutenberg).
-export([
         main/1,
         start/0,
         start/2,
         stop/0,
         stop/1
        ]).

main(_) ->
    check_needed_executables(),
    io:format("Hola").

start() ->
    {ok, _Started} = application:ensure_all_started(gutenberg).

stop() ->
    application:stop(gutenberg).

start(_StartType, _StartArgs) ->
    gute_sup:start_link().

stop(_State) ->
    ok.

needed_executables() ->
    [
     "git"
    ].

check_needed_executables() ->
    lists:foreach(
      fun (X) ->
              executable_present(X)
      end,
      needed_executables()).

executable_present(Name) ->
    case os:find_executable(Name) of
        false ->
            throw(Name ++ " is not present on the system");
        _ ->
            ok
    end.
