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
    check_version(),
    check_needed_executables(),
    thorerl:parse(gut_commands,
                  Args,
                  "https://github.com/unbalancedparentheses/gut/issues/new").

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

check_version() ->
    {Major, _} = version(),
    case 17 =< Major of
        true ->
            ok;
        false ->
            io:format("Erlang 17 or higher is needed. You are using Erlang ~p ~n", [Major]),
            halt(1)
    end.

version() ->
    version_tuple(erlang:system_info(otp_release)).

version_tuple(OtpRelease) ->
    case re:run(OtpRelease, "R?(\\d+)B?-?(\\d+)?", [{capture, all, list}]) of
        {match, [_Full, Maj, Min]} ->
            {list_to_integer(Maj), list_to_integer(Min)};
        {match, [_Full, Maj]} ->
            {list_to_integer(Maj), 0}
    end.
