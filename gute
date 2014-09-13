#!/usr/bin/env escript

main(Args) ->
    [Path | _] = Args,
    update_all_files(Path).

file_tree(Path) ->
    Result = filelib:wildcard("**/*", Path),
    ResultNoDir = lists:filter(fun (X) ->
                                       not filelib:is_dir(path(Path, X))
                               end, Result),
    lists:delete("goro.config", ResultNoDir).

update_all_files(Path) ->
    Files = file_tree(Path),
    io:format("Files ~p~n", [Files]),
    {ok, Patterns} = file:consult(filename:join(Path, "goro.config")),
    PatternValues = user_values(Patterns),
    lists:foreach(
      fun (File) ->
              Filename = filename:join(Path, File),
              update(Filename, PatternValues)
      end,
      Files).

user_values(Patterns) ->
    lists:foldl(
      fun ({Pattern, Message}, Acc) ->
              {ok, [Value]} = io:fread(Message, "~s"),
              [{Pattern, Value} | Acc]
      end,
      [],
      Patterns).

update(File, Patterns) ->
    lists:foreach(
      fun ({Variable, Value}) ->
              BValue = erlang:list_to_binary(Value),
              render(File, Variable, BValue)
      end, Patterns).

render(File, Variable, Value) ->
    {ok, Content} = file:read_file(File),
    NewContent = binary:replace(Content, Variable, Value, [global]),
    file:write_file(File, NewContent).

path(Path, File) ->
    filename:join(Path, File).
