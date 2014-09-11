#!/usr/bin/env escript

main(Args) ->
    [Path | _] = Args,
    update_all_files(Path).

file_tree(Path) ->
    Result = filelib:wildcard("**/*", Path),
    lists:delete("goro.config", Result).

update_all_files(Path) ->
    Files = file_tree(Path),
    io:format("Files ~p~n", [Files]),
    {ok, Patterns} = file:consult(filename:join(Path, "goro.config")),
    lists:foreach(
      fun (File) ->
              Filename = filename:join(Path, File),
              update(Filename, Patterns)
      end,
      Files).

update(File, Patterns) ->
    lists:foreach(
      fun ({VariableName, Variable}) ->
              {ok, [Value]} = io:fread("Yo mamita, muestrame esa colita ", "~s"),
              BValue = erlang:list_to_binary(Value),
              render(File, Variable, BValue)
      end, Patterns).


render(File, Variable, Value) ->
    {ok, Content} = file:read_file(File),
    NewContent = binary:replace(Content, Variable, Value, [global]),
    file:write_file(File, NewContent).
