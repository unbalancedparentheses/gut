-module(gut_config).
-export([
         run/1
        ]).

config_name() ->
  "gut.yaml".

exists(Path) ->
  FullPath = filename:join(Path, config_name()),
  filelib:is_file(FullPath).

run(Path) ->
  case exists(Path) of
    true ->
      #{"postinstall" := Commands} = get_yaml(Path),
      lists:map(fun (X) ->
                    gut_port:run(X, Path)
                end, Commands);
    false ->
      ok
  end.

get_yaml(Path) ->
  FullPath = filename:join(Path, config_name()),
  [[Mappings]] = yamerl_constr:file(FullPath),
  maps:from_list(Mappings).
