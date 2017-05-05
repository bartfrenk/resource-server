-module(overseer).
-export([start/1, stop/0 ,stop/1]).
-export([running/2]).

-define(NAME, overseer).
-define(TIMEOUT, 1000).

%% @doc Start the resource server under supervision.
start(ServerCount) ->
  spawn(fun() -> init(ServerCount) end).

spawn_server(I, StorePid) ->
  spawn_link(server, init, [I, StorePid]).

init(ServerCount) ->
  register(?NAME, self()), %% easier to refer to overseer
  process_flag(trap_exit, true),
  StorePid = spawn_link(store, init, []),
  ServerPids = lists:map(fun(I) -> spawn_server(I, StorePid) end,
                         lists:seq(1, ServerCount)),
  running(StorePid, ServerPids).

running(StorePid, ServerPids) ->
  log:debug(?NAME, "store: ~p, servers: ~p~n", [StorePid, ServerPids]),

  receive
    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {'EXIT', StorePid, Reason} ->
      log:info(?NAME, "store exited with reason ~p~n", [Reason]),
      NewStorePid = spawn_link(store, init, []),
      lists:map(fun(P) -> utils:call(P, {set_store, NewStorePid}) end,
                ServerPids),
      running(NewStorePid, ServerPids);

    {'EXIT', ProcessPid, Reason} ->
      case sets:is_element(ProcessPid, ServerPids) of
        true ->
          log:info(?NAME, "server exited with reason ~p~n", [Reason]),
          P = spawn_link(server, init, [StorePid]),
          NewServerPids = sets:add_element(P, sets:del_element(P, ServerPids)),
          running(StorePid, NewServerPids);
        false ->
          log:err(?NAME, "received exit signal ~p~n", [Reason])
        end

  end.

%% @doc Stops the overseer.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> utils:call_registered(?NAME, stop, Timeout).
