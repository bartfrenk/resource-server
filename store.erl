-module(store).
-export([start/0, start/1, init/0, init/1]).
-export([get/0, put/1, stop/0]).
-define(NAME, store).

resources() -> lists:seq(10, 20).

%% @doc Spawns the resource store.
start() -> start(resources()).
start(Resources) ->
  spawn(fun() -> init(Resources) end).

%% @doc Starts the resource store.
init() -> init(resources()).
init(Resources) ->
  register(?NAME, self()), %% easier to refer to overseer
  process_flag(trap_exit, true),
  running({Resources, #{}}).

running(Resources) ->
  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, {put, NewResources}} ->
      Pid ! {reply, Tag, ok},
      running(NewResources);

    {request, Tag, Pid, get} ->
      Pid ! {reply, Tag, Resources},
      running(Resources)
  end.

stop() -> utils:call_registered(?NAME, stop).

put(Resources) -> utils:call_registered(?NAME, {put, Resources}).

get() -> utils:call_registered(?NAME, get).


