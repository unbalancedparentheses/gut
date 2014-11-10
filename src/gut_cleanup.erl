-module(gut_cleanup).
-export([
         compiled/0,
         git/1,
         gitignore/1,
         config/1,
         readme/1,
         license/1
        ]).

compiled() ->
  os:cmd("rm -rf " ++ filename:join(gut_path:home(), "compiled")).

git(Path) ->
  os:cmd("rm -rf " ++ Path ++ "/.git").

gitignore(Path) ->
  os:cmd("rm " ++ filename:join(Path, ".gitignore")).

config(Path) ->
  os:cmd("rm " ++ filename:join(Path, gut_config:name())).

readme(Path) ->
  os:cmd("rm " ++ filename:join(Path, "README.md")).

license(Path) ->
  os:cmd("rm " ++ filename:join(Path, "LICENSE")).
