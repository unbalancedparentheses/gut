-module(goro_template).
-export([
         path/2
        ,load/2
        ,render/3
        ,write/5
        ]).

path(PluginName, FileName) ->
    filename:join([".", "plugins", PluginName, "templates", FileName]).

load(PluginName, FileName) ->
    erlang:element(2, file:read_file(path(PluginName, FileName))).

render(PluginName, FileName, Pattern) ->
    binary:replace(load(PluginName, FileName),
                   erltool_patterns:project(),
                   erlang:list_to_binary(Pattern),
                   [global]
                  ).

write(PluginName, FileName, Pattern, DestFile, DestPath) ->
    file:write_file(filename:join(DestPath, DestFile), render(PluginName, FileName, Pattern)).
