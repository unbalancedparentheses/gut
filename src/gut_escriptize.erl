-module(gut_escriptize).
-export([
         run/0,
         run/1
        ]).

run() ->
    {ok, Path} = file:get_cwd(),
    Dir = filename:basename(Path),
    run(Dir).

run(Name) ->
    %% Make sure file:consult can parse the .app file
    case file:consult("ebin/" ++ Name ++ ".app") of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("Invalid syntax in ebin/~s.app: ~p\n", [Name, Reason]),
            halt(1)
    end,

    %% Add ebin paths to our path
    true = code:add_path("ebin"),
    ok = code:add_paths(filelib:wildcard("deps/*/ebin")),

    %% Read the contents of the files in ebin(s)
    Files = lists:flatmap(fun(Dir) -> load_files(Dir) end,
                          ["ebin" | filelib:wildcard("deps/*/ebin")]),

    case zip:create("mem", Files, [memory]) of
        {ok, {"mem", ZipBin}} ->
            %% Archive was successfully created. Prefix that with
            %% header and write to Name file
            Header = <<"#!/usr/bin/env escript\n%%! "
                       "+Bc +K true -smp enable\n">>,
            Script = <<Header/binary, ZipBin/binary>>,
            case file:write_file(Name, Script) of
                ok -> ok;
                {error, WriteError} ->
                    io:format("Failed to write ~s: ~p\n", [Name, WriteError]),
                    halt(1)
            end;
        {error, ZipError} ->
            io:format("Failed to construct ~s archive: ~p\n", [Name, ZipError]),
            halt(1)
    end,

    %% Finally, update executable perms for our script
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ Name),
            ok;
        _ ->
            ok
    end,

    %% Add a helpful message
    io:format("Congratulations! You now have a self-contained script "
              "called ~s in your bin directory.\n", [Name]).

load_files(Dir) ->
    [read_file(Filename, Dir) || Filename <- filelib:wildcard("*", Dir)].

read_file(Filename, Dir) ->
    {ok, Bin} = file:read_file(filename:join(Dir, Filename)),
    {Filename, Bin}.
