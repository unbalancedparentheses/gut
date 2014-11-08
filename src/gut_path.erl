-module(gut_path).
-export([
         home/0,
         ensure_home/0,
         file_tree/1,
         file_tree/2,
         final_path/2
        ]).

home() ->
  os:getenv("HOME") ++ "/.gut".

ensure_home() ->
  LocalPath = gut_path:home(),
  ok = filelib:ensure_dir(LocalPath),
  file:make_dir(LocalPath).

file_tree(Path) ->
  Result = filelib:wildcard("**/*", Path),
  lists:filter(fun (X) ->
                   AbsX = filename:join(Path, X),
                   not filelib:is_dir(AbsX)
               end,
               Result).

file_tree(Path, full_path) ->
  Result = file_tree(Path),
  lists:map(fun (File) ->
                filename:join(Path, File)
            end,
            Result);
file_tree(Path, _) ->
  file_tree(Path).

final_path(DesiredPath, Cwd) ->
  case Cwd of
    true ->
      filename:dirname(filename:absname(DesiredPath));
    false->
      filename:absname(DesiredPath)
  end.
