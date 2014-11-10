-module(gut_compile).
-export([
         compile/3
        ]).

compile(FullGeneratorName, Name, PatternValues) ->
  Destination = copy_to_compiled(FullGeneratorName, Name),

  gut_readme:generate(Destination, Name),
  gut_cleanup:git(Destination),

  Files = gut_path:file_tree(Destination),
  lists:foreach(
    fun (File) ->
        FileFullPath = filename:join(Destination, File),
        update(FileFullPath, Name, PatternValues),
        print_generated(Name, File)
    end,
    Files),
  io:format("~n"),
  Destination.

copy_to_compiled(FullGeneratorName, Name) ->
  Home = gut_path:home(),

  gut_cleanup:compiled(),

  Destination = filename:join([Home, "compiled", Name]),
  filelib:ensure_dir(Destination),

  Source = filename:join(Home, FullGeneratorName),
  CopyCmd = io_lib:format("cp -a ~s ~s", [Source, Destination]),
  os:cmd(CopyCmd),
  Destination.

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

print_generated(Name, File) ->
  io:format(color:greenb("* creating ")),
  io:format("~s/~s~n", [Name, File]).
