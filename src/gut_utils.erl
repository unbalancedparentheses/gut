-module(gut_utils).
-export([
         loop_read/0
        ]).

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
