-module(overseer).
-export([start/0, stop/0 ,stop/1]).
-export([running/2]).

-define(NAME, overseer).
-define(TIMEOUT, 1000).

%% @doc Start the resource server under supervision.
start() ->
  spawn(fun() -> init() end).

init() ->
  register(?NAME, self()), %% easier to refer to overseer
  process_flag(trap_exit, true),
  StorePid = spawn_link(store, init, []),
  ServerPid = spawn_link(server, init, [StorePid]),
  running(StorePid, ServerPid).

running(StorePid, ServerPid) ->
  log:debug("overseer", "store: ~p, server: ~p~n", [StorePid, ServerPid]),

  receive
    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {'EXIT', ServerPid, Reason} ->
      log:info("overseer", "server exited with reason ~s~n", [Reason]),
      running(StorePid, spawn_link(server, init, [StorePid]));

    {'EXIT', StorePid, Reason} ->
      log:info("overseer", "store exited with reason ~s~n", [Reason]),
      NewStorePid = spawn_link(store, init, []),
      server:set_store(NewStorePid),
      running(NewStorePid, ServerPid)

  end.

%% @doc Stops the supervisor.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> utils:call_registered(?NAME, stop, Timeout).
