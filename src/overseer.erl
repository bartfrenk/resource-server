-module(overseer).
-export([start/1, stop/0 ,stop/1]).
-export([running/2]).
-export([index_of/2, replace/3]).

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
  StorePid = spawn_link(store, init, [ServerCount]),
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
      case index_of(ProcessPid, ServerPids) of
        none ->
          log:err(?NAME, "received exit signal ~p~n", [Reason]);
        I ->
          log:info(?NAME, "server ~p exited with reason ~p~n", [I, Reason]),
          P = spawn_link(server, init, [I, StorePid]),
          NewServerPids = replace(P, I, ServerPids),
          running(StorePid, NewServerPids)
        end

  end.

index_of(_E, [], _Index) -> none;
index_of(E, [E|_Es], Index) -> Index;
index_of(E, [_|Es], Index) -> index_of(E, Es, Index + 1).
index_of(E, Es) -> index_of(E, Es, 1).

replace(E, 1, [_|Es]) -> [E|Es];
replace(E, Index, [F|Es]) when Index > 1 -> [F|replace(E, Index - 1, Es)];
replace(_, _, _) -> [].

%% @doc Stops the overseer.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> utils:call_registered(?NAME, stop, Timeout).
