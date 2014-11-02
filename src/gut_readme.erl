-module(gut_readme).
-export([
         generate/2
        ]).

generate(Path, Name) ->
    ReadmePath = filename:join(Path, "README.md"),

    Content = io_lib:format("~s~n===~n", [Name]),
    file:write_file(ReadmePath, Content).
