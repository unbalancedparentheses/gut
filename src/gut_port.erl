-module(gut_port).
-behaviour(gen_server).

-export([
         start_link/0,
         run/2
        ]).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run(Command, WorkingDir) ->
  io:format("~s ~s~n", [color:greenb("* running"), Command]),
  {Status, Msgs} = gen_server:call(?MODULE, {run, Command, WorkingDir}, 45000),
  case Status of
    0 ->
      queue:to_list(Msgs);
    _ ->
      throw({error,
             "Execution of " ++
               Command ++
               " returned error code " ++
               erlang:integer_to_list(Status)
            })
  end.

%% Callback implementation
init([]) ->
  {ok, #{}}.

handle_info({_, {data, Msg}}, #{msgs := Queue} = State) ->
  io:format(Msg),
  {noreply, State#{msgs => queue:in(Msg, Queue)}};
handle_info({_, {exit_status, Status}}, #{from := From, msgs := Msgs}) ->
  gen_server:reply(From, {Status, Msgs}),
  {noreply, #{}}.

handle_call({run, Command, WorkingDir}, From, _State) ->
  open_port({spawn, Command}, [{cd, WorkingDir}, exit_status, stderr_to_stdout]),
  {noreply, #{from => From,
              msgs => queue:new(),
              code => 0
             }}.

handle_cast(_Msg, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
