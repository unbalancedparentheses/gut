-module(escriptize).

-export([escriptize/1]).
%% #!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -config rel/sys.config


-spec escriptize(string()) -> ok.
escriptize(ProjectName) ->
  % Make sure file:consult can parse the .app file
  case file:consult("./" ++ ProjectName ++ ".app") of
    {ok, _} ->
      ok;
    {error, Reason} ->
      io:format("Invalid syntax in ebin/~s.app: ~p\n", [ProjectName, Reason]),
      halt(1)
  end,

  % Add ebin paths to our path
  true = code:add_path("ebin"),
  ok   = code:add_paths(filelib:wildcard("deps/*/ebin")),

  % Read the contents of the files in ebin(s)
  LoadFun  = fun(Dir) -> load_files(Dir) end,
  DepEbins = filelib:wildcard("deps/*/ebin"),
  Files    = lists:flatmap(LoadFun, ["ebin" | DepEbins]),

  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      % Archive was successfully created.
      % Prefix that with header and write to file
      ScriptIOList =
        [ <<"#!/usr/bin/env escript\n%%! +Bc +K true -name ">>
        , list_to_binary(ProjectName)
        , <<" -smp enable\n", ZipBin/binary>>
        ],
      Script = iolist_to_binary(ScriptIOList),
      case file:write_file("bin/" ++ ProjectName, Script) of
        ok ->
          ok;
        {error, WriteError} ->
          io:format("Failed to write ~s: ~p\n", [ProjectName, WriteError]),
          halt(1)
      end;
    {error, ZipError} ->
      io:format("Failed to construct ~s archive: ~p\n", [ProjectName, ZipError]),
      halt(1)
  end,

  % Finally, update executable perms for our script
  case os:type() of
    {unix,_} ->
      [] = os:cmd("chmod u+x ./" ++ ProjectName),
      ok;
    _ ->
      ok
  end,

  % Add a helpful message
  CongratsMsg =
    "Congratulations! You now have a self-contained script called \""
    ++ ProjectName ++
    "\" in\nyour bin directory.\n",
  io:format(CongratsMsg).

load_files(Dir) ->
  [  read_file(Filename, Dir)
  || Filename <- filelib:wildcard("*", Dir)
  ].

read_file(Filename, Dir) ->
  {ok, Bin} = file:read_file(filename:join(Dir, Filename)),
  {Filename, Bin}.
