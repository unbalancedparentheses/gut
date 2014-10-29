-module(gut_thor).
-export([
         parse/2
        ]).

parse(Module, Args) ->
    try
        process(Module, Args)
    catch
        _:{error, Message} ->
            io:format("~s~n", [Message]),
            halt(1);
        _:Exception ->
            io:format("An unexpected error ocurred.~n"
                      "Please fill a new issue https://github.com/unbalancedparentheses/gut/issues/new~n~n"
                      "Error: ~p~n"
                      "Stacktrace:~n~p~n"
                      "Arguments: ~p~n",
                      [Exception, erlang:get_stacktrace(), Args]),
            halt(1)
    end.

process(Module, ["help" | Opts]) ->
    Help = Module:help(),

    print_help(Help, Opts);
process(Module, [Function| Opts]) ->
    Function2 = erlang:list_to_atom(Function),

    code:ensure_loaded(Module),
    Exported = erlang:function_exported(Module, Function2, 1),

    exported(Exported, Module, Function2, Opts).

exported(false, _, Function, Opts) ->
    io:format("Command ~p with arguments ~p is not supported ~n", [Function, Opts]),
    halt(1);
exported(true, Module, Function, Opts) ->
    ok = Module:Function(Opts).


print_help(Help, []) ->
    Padding = padding_size(maps:keys(Help)),
    maps:fold(fun (Key, #{desc := Desc}, _) ->
                      PaddedKey = string:left(Key, Padding),
                      ColorPaddedKey = color:red(PaddedKey),
                      io:format("~s # ~s~n", [ColorPaddedKey, Desc])
              end, 0, Help);
print_help(Help, [Command | _]) ->
    case maps:is_key(Command, Help) of
        true ->
            #{desc := Desc, long := Long} = maps:get(Command, Help),
            io:format("~s~n~n~s~n", [Desc, Long]);
        false ->
            throw({error, "No help entry for " ++ Command})
    end.

padding_size(List) ->
    lists:foldl(fun (X, Length) ->
                        largest(string:len(X), Length)
                end, 0, List).

largest(X, Y) when X < Y ->
    Y;
largest(X, _Y) ->
    X.
