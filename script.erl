#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -config rel/sys.config
main(_Args) ->
  %% Make sure file:consult can parse the .app file
  case file:consult("ebin/gossip.app") of
    {ok, _} ->
      ok;
    {error, Reason} ->
      io:format("Invalid syntax in ebin/gossip.app: ~p\n", [Reason]),
      halt(1)
  end,

  %% Add ebin paths to our path
  true = code:add_path("ebin"),
  ok = code:add_paths(filelib:wildcard("deps/*/ebin")),

  %% Read the contents of the files in ebin(s)
  Files = lists:flatmap(fun(Dir) -> load_files(Dir) end, ["ebin"|filelib:wildcard("deps/*/ebin")]),

  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      %% Archive was successfully created. Prefix that with header and write to "gossip" file
      Script = <<"#!/usr/bin/env escript\n%%! +Bc +K true -name gossip  -smp enable\n", ZipBin/binary>>,
      case file:write_file("bin/gossip", Script) of
        ok -> ok;
        {error, WriteError} ->
          io:format("Failed to write bin/gossip: ~p\n", [WriteError]),
          halt(1)
      end;
    {error, ZipError} ->
      io:format("Failed to construct bin/gossip archive: ~p\n", [ZipError]),
      halt(1)
  end,

  %% Finally, update executable perms for our script
  case os:type() of
    {unix,_} ->
      [] = os:cmd("chmod u+x bin/gossip"),
      ok;
    _ ->
      ok
  end,

  %% Add a helpful message
  io:format("Congratulations! You now have a self-contained script called \"gossip\" in\n"
              "your bin directory.\n").

load_files(Dir) ->
  [read_file(Filename, Dir) || Filename <- filelib:wildcard("*", Dir)].

read_file(Filename, Dir) ->
  {ok, Bin} = file:read_file(filename:join(Dir, Filename)),
  {Filename, Bin}.
