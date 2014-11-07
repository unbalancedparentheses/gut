-module(gut_compile).
-export([
         compile/3
        ]).

conf() ->
  "gut.conf".

compile(FullGeneratorName, Name, PatternValues) ->
  Destination = copy_to_compiled(FullGeneratorName, Name),

  gut_readme:generate(Destination, Name),
  os:cmd("rm -rf " ++ Destination ++ "/.git"),

  Files = file_tree(Destination),
  lists:foreach(
    fun (File) ->
        update(File, Name, PatternValues)
    end,
    Files),
  Destination.

copy_to_compiled(FullGeneratorName, Name) ->
  Home = gut:home(),

  os:cmd("rm -rf " ++ filename:join(Home, "compiled")),

  Destination = filename:join([Home, "compiled", Name]),
  filelib:ensure_dir(Destination),

  Source = filename:join(Home, FullGeneratorName),
  CopyCmd = io_lib:format("cp -a ~s ~s", [Source, Destination]),
  os:cmd(CopyCmd),
  Destination.

file_tree(Path) ->
  Result = filelib:wildcard("**/*", Path),
  FullPathResult = lists:map(fun (File) ->
                                 filename:join(Path, File)
                             end,
                             Result),
  ResultNoDir = lists:filter(fun (X) ->
                                 (not filelib:is_dir(X))
                                   and not is_conf(X)
                             end,
                             FullPathResult),
  lists:delete(conf(), ResultNoDir).

update(File, ProjectName, Patterns) ->
  lists:foreach(
    fun ({Variable, Value}) ->
        BValue = erlang:list_to_binary(Value),
        render(File, Variable, BValue)
    end,
    Patterns),
  rename(File, ProjectName).

render(File, Variable, Value) ->
  {ok, Content} = file:read_file(File),
  NewContent = binary:replace(Content, Variable, Value, [global]),
  file:write_file(File, NewContent).

rename(FileName, Value) ->
  NewFilename = erlang:iolist_to_binary(re:replace(FileName, "name", Value)),
  file:rename(FileName, NewFilename),
  NewFilename.

is_conf(Path) ->
  case re:run(Path, [".*", conf(), ".*"]) of
    nomatch ->
      false;
    _ ->
      true
  end.
