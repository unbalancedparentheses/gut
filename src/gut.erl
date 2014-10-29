-module(gut).
-export([
         start/0,
         start/2,
         stop/0,
         stop/1,
         main/1
        ]).

%%% Exported functions
start() ->
    {ok, _Started} = application:ensure_all_started(gut).

stop() ->
    application:stop(gut).

start(_StartType, _StartArgs) ->
    gut_sup:start_link().

stop(_State) ->
    ok.

main(Args) ->
    gut:start(),
    check_needed_executables(),
    gut_thor:parse(gut_commands, Args).

%% Executables checks
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
            io:format(Name ++ " is not present on the system~n"),
            halt(1);
        _ ->
            ok
    end.
