-module(overseer).
-export([start/1, stop/0 ,stop/1]).
-export([running/2]).

-define(NAME, overseer).
-define(TIMEOUT, 1000).

%% @doc Start the resource server under supervision.
start(InitFn) ->
  spawn(fun() -> init(InitFn) end).

%% @private
init(InitFn) ->
  register(?NAME, self()), %% easier to refer to supervisor
  process_flag(trap_exit, true),
  Supervised = spawn_link(InitFn),
  running(InitFn, Supervised).

running(InitFn, Child) ->

  receive
    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {'EXIT', Child, Reason} ->
      log:info("supervisor", "process ~p exited with reason ~s~n",
               [Child, Reason]),
      running(InitFn, spawn_link(InitFn))

  end.

%% @doc Stops the supervisor.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> call_supervisor(stop, Timeout).

%% @private
call_supervisor(Request, Timeout) ->
  case whereis(?NAME) of
    undefined -> {error, no_server};
    Pid -> utils:call(Pid, Request, Timeout)
  end.

