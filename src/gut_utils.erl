-module(gut_utils).
-export([
         loop_read/0,
         maps_get/3
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


maps_get(Key, Map, Default) ->
  try
    maps:get(Key, Map)
  catch
    _:bad_key ->
      Default
  end.
