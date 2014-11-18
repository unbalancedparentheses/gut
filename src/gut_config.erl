-module(gut_config).
-export([
         name/0,
         run/2,
         get_yaml/1
        ]).

name() ->
  "gut.yaml".

exists(Path) ->
  FullPath = filename:join(Path, name()),
  filelib:is_file(FullPath).

run(CompiledPath, DesiredPath) ->
  case exists(CompiledPath) of
    true ->
      Yaml = get_yaml(CompiledPath),
      postinstall(Yaml, CompiledPath),

      #{"cwd" := Cwd} = Yaml,
      cleanup(CompiledPath, Cwd),
      gut_path:final_path(DesiredPath, Cwd);
    false ->
      gut_path:final_path(DesiredPath, false)
  end.

cleanup(Path, Cwd) ->
  gut_cleanup:config(Path),

  case Cwd of
    true ->
      gut_cleanup:readme(Path),
      gut_cleanup:license(Path),
      gut_cleanup:gitignore(Path);
    false ->
      ok
  end.

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
  gut_utils:loop_read().

run_comands(Commands, Path) ->
  lists:map(fun (X) ->
                gut_port:run(X, Path)
            end, Commands).

message(#{"message" := Message}) ->
  io:format("~n" ++ color:greenb("Message from generator:")),
  io:format("~n~s~n", [Message]);
message(_) ->
  ok.

get_yaml(Path) ->
  FullPath = filename:join(Path, name()),
  case read_yaml(FullPath) of
    [Mappings] ->
      yaml_to_map(Mappings);
    _ ->
      ok
  end.

yaml_to_map(Mappings) ->
  Result = maps:from_list(Mappings),

  Postinstall = gut_utils:maps_get("postinstall", Result, undefined),

  Cwd = case gut_utils:maps_get("cwd", Result, false) of
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
