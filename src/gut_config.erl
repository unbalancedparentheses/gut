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
      Yaml = get_yaml(Path),
      postinstall(Yaml, Path);
    false ->
      ok
  end.

postinstall(#{"postinstall" := Postinstall}, Path) ->
  commands(Postinstall, Path),
  message(Postinstall);
postinstall(_, _) ->
  ok.

commands(#{"commands" := Commands}, Path) ->
  lists:map(fun (X) ->
                gut_port:run(X, Path)
            end, Commands);
commands(_, _ ) ->
  ok.

message(#{"message" := Message}) ->
  io:format("~n~s~n", [color:greenb(Message)]);
message(_) ->
  ok.

get_yaml(Path) ->
  FullPath = filename:join(Path, config_name()),
  case yamerl_constr:file(FullPath) of
    [Mappings] ->
      yaml_to_map(Mappings);
    _ ->
      ok
  end.

yaml_to_map(Mappings) ->
  Result = maps:from_list(Mappings),

  Postinstall = maps:get("postinstall", Result, undefined),
  case Postinstall of
    undefined ->
      Result;
    _ ->
      Result#{"postinstall" := maps:from_list(Postinstall)}
  end.
