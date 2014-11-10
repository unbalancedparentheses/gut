-module(gut_generators).

-export([
         find_all/0,
         find_all_by_name/1,
         find_by_name/1,
         clone/2,
         copy/2,
         update/0
        ]).

-type generator() :: #{name => binary(),
                       url => binary(),
                       clone_url => binary(),
                       description => binary(),
                       stars => binary()
                      }.

%%% Exported

-spec find(integer()) -> [generator()].
find(Page) ->
  {ok, Resp} = github_search(gut_suffix:suffix(), Page),
  RespBinary = erlang:list_to_binary(Resp),
  #{<<"items">> := Items} = jsxn:decode(RespBinary),
  lists:map(fun item_to_generator/1, Items).

-spec item_to_generator(map()) -> generator().
item_to_generator(Item) ->
  #{<<"clone_url">> := CloneUrl,
    <<"html_url">> := Url,
    <<"name">> := Name,
    <<"description">> := Description,
    <<"stargazers_count">> := Stars,
    <<"owner">> := Owner
   } = Item,
  #{<<"login">> := User} = Owner,

  #{url => Url,
    clone_url => CloneUrl,
    name => Name,
    description => Description,
    stars => erlang:integer_to_binary(Stars),
    user => User
   }.

find_all_by_name(FindName) ->
  All = find_all(),
  lists:filter(fun (#{name := GenNameBin}) ->
                   GenName = erlang:binary_to_list(GenNameBin),
                   case re:run(GenName, FindName) of
                     nomatch ->
                       false;
                     _ ->
                       true
                   end
               end,
               All).

-spec find_by_name(string()) -> generator().
find_by_name(Name) ->
  All = find_all(),
  Result = [Generator || Generator = #{name := GenName} <- All,
                         binary_to_list(GenName) == Name],
  case Result of
    [] -> not_found;
    [Generator | _] -> Generator
  end.

-spec find_all() -> [generator()].
find_all() ->
  PreFiltering = find_all(1, []),
  Filtered = filter(PreFiltering),
  sort_by_stars(Filtered).

-spec find_all(integer(), [generator()]) -> [generator()].
find_all(Page, Results) ->
  case find(Page) of
    [] ->
      Results;
    MoreResults ->
      find_all(Page + 1, Results ++ MoreResults)
  end.

-spec clone(binary(), binary()) -> ok | {error, term}.
clone(GenName, GenCloneUrl) ->
  LocalDir = filename:join(gut_path:home(), GenName),
  case file_exists(LocalDir) of
    false ->
      CloneCmd = io_lib:format("git clone ~s ~s", [GenCloneUrl, LocalDir]),
      os:cmd(CloneCmd);
    true ->
      ok
  end.

-spec copy(binary(), string()) -> ok.
copy(Path, Destination) ->
  Files = gut_path:file_tree(Path),

  lists:foreach(fun(File) ->
                    SourceFile = filename:join(Path, File),
                    DestFile = filename:join(Destination, File),

                    ok = filelib:ensure_dir(DestFile),
                    {ok, _} = file:copy(SourceFile, DestFile),
                    print_generated(File)
                end, Files).

-spec update() -> ok.
update() ->
  GeneratorsDirs = filelib:wildcard(gut_path:home() ++ "/*"),
  PullFun = fun(DirPath) ->
                file:set_cwd(DirPath),
                os:cmd("git reset --hard HEAD"),
                os:cmd("git clean -f"),
                os:cmd("git pull")
            end,
  lists:foreach(PullFun, GeneratorsDirs).

%% internal
-spec github_search(string(), integer()) -> {ok, binary()} | {error, term()}.
github_search(Query, Page) ->
  QueryUrl = "https://api.github.com/search/repositories?q=~s&page=~p",
  Url = io_lib:format(QueryUrl, [Query, Page]),

  Options = [{ssl_options, [{depth, 0}]}],
  Headers = [{"User-Agent", "Gutenberg"}],
  Body = [],
  case ibrowse:send_req(Url, Headers, get, Body, Options) of
    {ok, "200", _RespHeaders, RespBody} ->
      {ok, RespBody};
    {ok, Status, RespHeaders, RespBody} ->
      {error, {Status, RespHeaders, RespBody}}
  end.

%% @doc Takes all results from the github search
%%      and keeps only the ones with the suffx.
-spec filter([generator()]) -> [generator()].
filter(List) ->
  lists:filter(fun (#{name := Name}) ->
                   gut_suffix:has_suffix(Name)
               end, List).

sort_by_stars(List) ->
  Sort = fun(#{stars := AStars}, #{stars := BStars}) ->
             AStars >= BStars
         end,
  lists:sort(Sort, List).

file_exists(Path) ->
  [] /= filelib:wildcard(Path).

print_generated(File) ->
  io:format(color:greenb("* creating ")),
  io:format("~s~n", [File]).
