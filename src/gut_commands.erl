%% -*- coding: utf-8 -*-
-module(gut_commands).
-export([
         help/0,
         version/1,
         new/1,
         search/1,
         escriptize/1,
         update/1,
         implode/1
        ]).

%% Commands
help() ->
  #{
     "version" =>
       #{desc => "Prints gutenberg version",
         long => ""
        },
     "new" => #{desc => "Creates a new project or file",
                long => ""
               },
     "search" => #{desc => "Search available generators",
                 long => ""
                },
     "escriptize" => #{desc => "Turn your erlang application into an escript",
                       long => ""
                      },
     "update" => #{desc => "Get the latest version of the gut executable and update.gens",
                   long => ""
                  },
     "help" => #{desc => "Prints help information",
                 long => ""
                },
     "implode" => #{desc => "Removes gut and all its associated files",
                 long => ""
                }
   }.

version(_) ->
  Ascii =
    <<"                __\n   ____ ___  __/ /_\n  / __ `/ / / / __/\n / /_/ / /_/ / /_\n \\__, /\\__,_/\\__/\n/____/\n">>,
  io:format(Ascii),
  io:format("Version: ~s~n", [version()]),
  ok.

version() ->
  {ok, Keys} = application:get_all_key(gut),
  proplists:get_value(vsn, Keys).

new([ProvidedName, DesiredPath | _]) ->
  FullGeneratorName = gut_suffix:full_name(ProvidedName),

  Name = filename:basename(DesiredPath),
  Values = [{<<"{{NAME}}">>, Name}],

  Generator = gut_generators:find_by_name(FullGeneratorName),
  case Generator of
    not_found ->
      io:format("Generator ~s not found ~n", [ProvidedName]);
    #{clone_url := GenCloneUrl,
      url := Url
     } ->
      io:format("Cloning ~s hosted at ~s~n", [color:greenb(ProvidedName), color:greenb(Url)]),
      io:format("Please submit a github issue if you find any problem with this generator~n"),

      gut_generators:clone(FullGeneratorName, GenCloneUrl),

      CompiledPath = gut_compile:compile(FullGeneratorName, Name, Values),

      Yaml = gut_config:get_yaml(CompiledPath),
      #{"cwd" := Cwd} = Yaml,
      gut_config:cleanup(CompiledPath, Cwd),
      FinalPath = gut_path:final_path(DesiredPath, Cwd),

      gut_generators:copy(CompiledPath, FinalPath),

      gut_config:postinstall(Yaml, FinalPath),

      io:format("~nThe job is done, boss.~n")
  end,
  ok;
new(_) ->
  throw({error, "Missing generator name"}).

search([]) ->
  io:format("Fetching list of generators from github...~n"),
  Generators = gut_generators:find_all(),
  print_generators(Generators),
  ok;
search([Name | _]) ->
  Generators = gut_generators:find_all_by_name(Name),
  print_generators(Generators),
  ok.

escriptize([]) ->
  gut_escriptize:run(),
  ok;
escriptize([Name | _]) ->
  gut_escriptize:run(Name),
  ok.

update(_) ->
  io:format("Updating generators..."),
  gut_generators:update(),
  io:format(" ~s~n", [color:greenb("done")]),

  ScriptPath = escript:script_name(),
  io:format("Updating gut binary in ~s...", [ScriptPath]),
  Url = "https://raw.githubusercontent.com/"
    "unbalancedparentheses/gut/master/bin/gut",
  {ok, "200", _, Content} = ibrowse:send_req(Url, [], get),
  file:write_file(ScriptPath, Content),
  io:format(" ~s~n", [color:greenb("done")]),
  ok.

implode(_) ->
  io:format("About to remove gut and all its associated files~n"),

  case gut_utils:loop_read() of
    true ->
      io:format("Hasta la vista baby~n"),
      ScriptPath = escript:script_name(),
      os:cmd("rm " ++ ScriptPath),

      Home = gut_path:home(),
      os:cmd("rm -rf " ++ Home),
      ok;
    false ->
      io:format("Chicken... bock, bock, bock, bock, bock, begowwwwk!~n"),
      ok
  end.
%% internal
print_generators(Generators) ->
  ColNames = #{name => <<"NAME">>,
               description => <<"DESCRIPTION">>,
               owner => <<"OWNER">>,
               stars => <<"STARS">>
              },
  Generators2 =  [ColNames| Generators],

  Padding = padding_size(Generators2),

  print_generator(ColNames, Padding),

  lists:foreach(fun (X) ->
                    print_generator(X, Padding)
                end,
                Generators).

print_generator(#{name := GenName,
                  description := Desc,
                  owner := Owner,
                  stars := Stars
                 },
                [NamePadSize, DescPadSize, OwnerPadSize, StarsPadSize]) ->
  ShortName = gut_suffix:short_name(GenName),
  ShortName2 = erlang:binary_to_list(ShortName),

  ShortNamePadded = string:left(ShortName2, NamePadSize),
  DescPadded = string:left(erlang:binary_to_list(Desc), DescPadSize),
  OwnerPadded = string:left(erlang:binary_to_list(Owner), OwnerPadSize),
  StarsPadded = string:right(erlang:binary_to_list(Stars), StarsPadSize),

  io:format("~s ~s   ~s ~s~n",
            [color:green(ShortNamePadded), DescPadded, OwnerPadded, StarsPadded]).

padding_size(List) ->
  SizeList = lists:foldl(fun (#{name := Name,
                                description := Desc,
                                owner := Owner,
                                stars := Stars
                               }, [NameList, DescList, OwnerList, StarsList]) ->
                             ShortName = gut_suffix:short_name(Name),

                             NameSize =erlang:size(ShortName),
                             DescSize =erlang:size(Desc),
                             OwnerSize =erlang:size(Owner),
                             StarsSize =erlang:size(Stars),

                             NameList2 = [NameSize | NameList],
                             DescList2 = [DescSize | DescList],
                             OwnerList2 = [OwnerSize | OwnerList],
                             StarsList2 = [StarsSize | StarsList],

                             [NameList2, DescList2, OwnerList2, StarsList2]
                         end, [[], [], [], []], List),

  lists:map(fun(X) ->
                lists:max(X)
            end, SizeList).
