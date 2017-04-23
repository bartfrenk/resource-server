-module(store).
-export([start/0, start/1, init/0, init/1]).
-export([get/1, put/2, stop/1]).

resources() -> lists:seq(10, 20).

%% @doc Spawns the resource store.
start() -> start(resources()).
start(Resources) ->
  spawn(fun() -> init(Resources) end).

%% @doc Starts the resource store.
init() -> init(resources()).
init(Resources) ->
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

stop(Store) -> utils:call(Store, stop).

put(Store, Resources) -> utils:call(Store, {put, Resources}).

get(Store) -> utils:call(Store, get).


