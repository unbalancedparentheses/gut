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
      postinstall(Yaml, Path),
      cleanup(Path),
      Yaml;
    false ->
      #{"cwd" => true}
  end.

cleanup(Path) ->
  os:cmd("rm -rf " ++ filename:join(Path, config_name())).

postinstall(#{"postinstall" := Postinstall}, Path) ->
  commands(Postinstall, Path),
  message(Postinstall);
postinstall(_, _) ->
  ok.

commands(#{"commands" := Commands}, Path) ->
  case comfirm_commands(Commands) of
    true ->
      run_comands(Commands, Path);
    false ->
      ok
  end;
commands(_, _ ) ->
  ok.

comfirm_commands(Commands) ->
  io:format("The generator wants to run the following list of commands:~n"),
  lists:foldl(fun (X, Acc) ->
                  io:format("~p. ~s~n", [Acc, X]),
                  Acc + 1
              end, 1, Commands),
  loop_read().

loop_read() ->
  Result = io:fread("Are you sure you want to continue: [y/n] ", "~c"),
  case Result of
    {ok, ["y"]} ->
      true;
    {ok, _} ->
      false;
    _ ->
      loop_read()
  end.

run_comands(Commands, Path) ->
  lists:map(fun (X) ->
                gut_port:run(X, Path)
            end, Commands).

message(#{"message" := Message}) ->
  io:format("~n~s~n", [color:greenb(Message)]);
message(_) ->
  ok.

get_yaml(Path) ->
  FullPath = filename:join(Path, config_name()),
  case read_yaml(FullPath) of
    [Mappings] ->
      yaml_to_map(Mappings);
    _ ->
      ok
  end.

yaml_to_map(Mappings) ->
  Result = maps:from_list(Mappings),

  Postinstall = maps:get("postinstall", Result, undefined),

  Cwd = case maps:get("cwd", Result, false) of
          true ->
            true;
          _ ->
            false
        end,

  case Postinstall of
    undefined ->
      Result;
    _ ->
      Result#{"postinstall" := maps:from_list(Postinstall),
              "cwd" =>
                Cwd
             }
  end.

read_yaml(Path) ->
  try
    yamerl_constr:file(Path)
  catch
    _:{yamerl_exception, [{_, _, Message, _, _, _, _, _}]} ->
      throw({error, "YAML error: " ++ Message})
  end.
