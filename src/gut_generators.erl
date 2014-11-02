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
                       description => binary()}.

%%% Exported

-spec find(integer()) -> [generator()].
find(Page) ->
  {ok, Resp} = github_search(gut_suffix:suffix(), Page),
  RespBinary = erlang:list_to_binary(Resp),
  #{<<"items">> := Items} = jsxn:decode(RespBinary),
  lists:map(fun item_to_generator/1, Items).

-spec item_to_generator(map()) -> generator().
item_to_generator(Item) ->
  #{<<"clone_url">> := Url,
    <<"name">> := Name,
    <<"description">> := Description,
    <<"stargazers_count">> := Stars,
    <<"owner">> := Owner
   } = Item,

  #{<<"login">> := User} = Owner,

  #{url => Url,
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
clone(GenName, GenUrl) ->
  ensure_local_dir(),
  NameStr = binary_to_list(GenName),
  LocalDir = local_dir_path() ++ "/" ++ NameStr,
  case file_exists(LocalDir) of
    false ->
      CloneCmd = io_lib:format("git clone ~s ~s", [GenUrl, LocalDir]),
      os:cmd(CloneCmd);
    true ->
      ok
  end.

-spec copy(binary(), string()) -> ok.
copy(GenName, Destination) ->
  NameStr = binary_to_list(GenName),
  LocalDir = local_dir_path() ++ "/" ++ NameStr,
  case file_exists(Destination) of
    false ->
      filelib:ensure_dir(Destination),
      CopyCmd = io_lib:format("cp -a ~s ~s", [LocalDir, Destination]),
      os:cmd(CopyCmd);
    true ->
      Message = "Folder " ++ Destination ++ " is already present",
      throw({error, Message})
  end.

-spec update() -> ok.
update() ->
  GeneratorsDirs = filelib:wildcard(local_dir_path() ++ "/*"),
  PullFun = fun(DirPath) ->
                file:set_cwd(DirPath),
                os:cmd("git reset --hard HEAD"),
                os:cmd("git clean -f"),
                os:cmd("git pull")
            end,
  lists:foreach(PullFun, GeneratorsDirs).

%%% Internal

ensure_local_dir() ->
  LocalPath = local_dir_path(),
  ok = filelib:ensure_dir(LocalPath),
  file:make_dir(LocalPath).

local_dir_path() ->
  os:getenv("HOME") ++ "/.gut".

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
%%      and keeps only the ones with the suffix.
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
