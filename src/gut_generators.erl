-module(gut_generators).

-export([
         find/1,
         find_all/0,
         find_all_by_name/1,
         find_by_name/1,
         clone/2,
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
item_to_generator(#{<<"clone_url">> := Url,
                    <<"name">> := Name,
                    <<"description">> := Description}) ->
    #{url => Url,
      name => Name,
      description => Description}.

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
    filter(PreFiltering).

-spec find_all(integer(), [generator()]) -> [generator()].
find_all(Page, Results) ->
    case find(Page) of
        [] ->
            Results;
        MoreResults ->
            find_all(Page + 1, Results ++ MoreResults)
    end.

-spec clone(generator(), string()) -> ok | {error, term}.
clone(#{name := Name, url := Url}, Destination) ->
    ensure_local_dir(),
    NameStr = binary_to_list(Name),
    LocalDir = local_dir_path() ++ "/" ++ NameStr,
    case file_exists(LocalDir) of
        false ->
            CloneCmd = io_lib:format("git clone ~s ~s", [Url, LocalDir]),
            os:cmd(CloneCmd);
        true ->
            ok
    end,
    case file_exists(Destination) of
        false ->
            CopyCmd = io_lib:format("cp -avr ~s ~s", [LocalDir, Destination]),
            os:cmd(CopyCmd);
        true ->
            throw({eexist, Destination})
    end.

-spec update() -> ok.
update() ->
    GeneratorsDirs = filelib:wildcard(local_dir_path() ++ "/*"),
    PullFun = fun(DirPath) ->
                      file:set_cwd(DirPath),
                      os:cmd("touch \"`date`\""),
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

file_exists(Path) ->
    [] /= filelib:wildcard(Path).
