-module(gut_thor).
-export([
         process/2
        ]).

process(Module, [Function| Opts]) ->
    Function2 = erlang:list_to_atom(Function),

    code:ensure_loaded(Module),
    Exported = erlang:function_exported(Module, Function2, 1),

    exported(Exported, Module, Function2, Opts).

exported(false, _, Function, Opts) ->
    io:format("Command ~p with arguments ~p is not supported ~n", [Function, Opts]),
    halt(1);
exported(true, Module, Function, Opts) ->
    try
        ok = Module:Function(Opts)
    catch
        _:{error, Message} ->
            io:format("~s~n", [Message]),
            halt(1);
        _:Exception ->
            io:format("An unexpected error ocurred.~n"
                      "Please fill a new issue https://github.com/unbalancedparentheses/gut/issues/new~n~n"
                      "Error: ~p~n"
                      "Stacktrace:~n~p~n"
                      "Function: ~p~n"
                      "Opts: ~p~n",
                      [Exception, erlang:get_stacktrace(), Function, Opts]),
            halt(1)
    end.
