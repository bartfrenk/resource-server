-module(client).
-export([start/1, stop/1, stop/2, pause/1, pause/2, continue/1, continue/2]).
-export([inc/0, inc/1, range/1, range/3, unit/0, delay/0, delay/1]).
-export([debug/3]).
-export([print/1, send/1]).
-export([compose/1, compose/2]).

-define(TIMEOUT, 1000).


%% === Running a client ===

start(Behaviour) ->
  spawn(fun() -> init(Behaviour) end).

%% @private
init(Behaviour) ->
  process_flag(trap_exit, true),
  running(Behaviour).

%% @private
running({Action, Delay, IsFinal, State}=Behaviour) ->
  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, continue} ->
      Pid ! {reply, Tag, continued},
      running(Behaviour);

    {request, Tag, Pid, pause} ->
      Pid ! {reply, Tag, paused},
      paused(Behaviour)
    after Delay ->
        NewState = Action(State),
        case IsFinal(NewState) of
          true -> ok;
          false -> running({Action, Delay, IsFinal, NewState})
        end
  end.

paused(Behaviour) ->
  receive
    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};
    {request, Tag, Pid, continue} ->
      Pid ! {reply, Tag, continued},
      running(Behaviour);
    {request, Tag, Pid, pause} ->
      Pid ! {reply, Tag, paused},
      paused(Behaviour)
  end.


%% === Synchronous client functions ===

stop(Client, Timeout) -> utils:call(Client, stop, Timeout).
stop(Client) -> stop(Client, ?TIMEOUT).

pause(Client, Timeout) -> utils:call(Client, pause, Timeout).
pause(Client) -> pause(Client, ?TIMEOUT).

continue(Client, Timeout) -> utils:call(Client, continue, Timeout).
continue(Client) -> continue(Client, ?TIMEOUT).


%% === Primitive behaviours ===

%% @doc Client does nothing.
unit() -> {fun(S) -> S end, 0, fun(_) -> false end, unit}.

%% @doc Client increments its integral state by K.
inc() -> inc(1).
inc(K) -> {fun(N) -> N + K end, 0, fun(_) -> false end, 0}.

%% @doc Client iterates over specified range.
range(Hi) -> range(1, Hi, 1).
range(Lo, Hi, Step) ->
  {fun(N) -> N + Step end, 0, fun(N) -> N > Hi end, Lo}.

%% @doc Slow down and print state transitions.
debug(Label, T, Behaviour) ->
  client:compose([client:delay(T), Behaviour, client:print(Label ++ ": ~p~n")]).

%% @doc Client waits T milliseconds before utils:calling itself.
delay() -> delay(1000).
delay(T) -> {fun(_) -> ok end, T}.

%% @doc Client prints its state.
print(Fmt) -> {fun(S) -> io:format(Fmt, [S]) end, 0}.

%% @doc Client sends its state to process Pid.
send(Pid) ->
  {fun(S) -> Pid ! S, S end, 0}.


%% === Behaviour combinators ===

compose({Effect, D1}, {Action, D2, IsFinal, State}) ->
  {fun(S) -> NewS = Action(S),
             Effect(NewS),
             NewS end,
   D1 + D2, IsFinal, State};
compose({Action, D1, IsFinal, State}, {Effect, D2}) ->
  {fun(S) -> Effect(S), Action(S) end, D1 + D2, IsFinal, State};
compose({A1, D1, I1, _S1}, {A2, D2, I2, S2}) ->
  {fun(S) -> A1(A2(S)) end, D1 + D2, fun(S) -> I1(S) or I2(S) end, S2};
compose({E1, D1}, {E2, D2}) ->
  {fun(S) -> E2(S), E1(S) end, D1 + D2}.

compose(Behaviours) ->
  lists:foldl(fun(B, Acc) -> compose(Acc, B) end, unit(), Behaviours).
