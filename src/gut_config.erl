-module(gut_config).
-export([
         run/1,
         get_yaml/1
        ]).

config_name() ->
  "gut.yaml".

exists(Path) ->
  FullPath = filename:join(Path, config_name()),
  filelib:is_file(FullPath).

run(Path) ->
  case exists(Path) of
    true ->
      #{"postinstall" := Postinstall} = get_yaml(Path),
      #{"commands" := Commands, "message" := Message} = Postinstall,
      lists:map(fun (X) ->
                    gut_port:run(X, Path)
                end, Commands),
      io:format("~n~s~n", [color:greenb(Message)]);
    false ->
      ok
  end.

get_yaml(Path) ->
  FullPath = filename:join(Path, config_name()),
  [Mappings] = yamerl_constr:file(FullPath),
  Result = maps:from_list(Mappings),
  #{"postinstall" := Postinstall} = Result,
  Result#{"postinstall" := maps:from_list(Postinstall)}.
