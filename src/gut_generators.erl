-module(gut_generators).

-export([
         find/1,
         find_all/0,
         find_by_name/1,
         clone/2,
         github_search/2
        ]).

-type generator() :: #{name => binary(),
                       url => binary(),
                       description => binary()}.

-spec find(integer()) -> [generator()].
find(Page) ->
    {ok, Resp} = github_search(gut_search_query(), Page),
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
    find_all(1, []).

-spec find_all(integer(), [generator()]) -> [generator()].
find_all(Page, Results) ->
    case find(Page) of
        [] ->
            Results;
        MoreResults ->
            find_all(Page + 1, Results ++ MoreResults)
    end.

-spec clone(generator(), string()) -> ok | {error, term}.
clone(#{url := Url}, Destination) ->
    Cmd= io_lib:format("git clone ~s ~s", [Url, Destination]),
    os:cmd(Cmd).

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

-spec gut_search_query() -> string().
gut_search_query() -> "gutenberg-generator".
