-module(store).
-export([start/1, init/1, running/1]).
-export([get/0, put/1, stop/0, upgrade/0]).
-export([resources/1]).
-define(NAME, store).

resources(ServerCount) -> resources(10, ServerCount).
resources(PoolSize, ServerCount) ->
  L = lists:map(fun(I) ->
                    {I, {lists:seq((I - 1) * PoolSize + 1, I * PoolSize), #{}}} end,
            lists:seq(1, ServerCount)),
  maps:from_list(L).

%% @doc Spawns the resource store.
start(ServerCount) ->
  spawn(fun() -> init(ServerCount) end).

%% @doc Starts the resource store.
init(ServerCount) ->
  register(?NAME, self()), %% easier to refer to overseer
  running(resources(ServerCount)).

running(Resources) ->
  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, {put, I, NewResources}} ->
      Pid ! {reply, Tag, ok},
      running(maps:put(I, NewResources, Resources));

    {request, Tag, Pid, {get, I}} ->
      Pid ! {reply, Tag, {ok, maps:get(I, Resources)}},
      running(Resources);

    {request, Tag, Pid, get} ->
      Pid ! {reply, Tag, {ok, Resources}},
      running(Resources);

    {request, Tag, Pid, upgrade} ->
      compile:file(?MODULE),
      code:soft_purge(?MODULE),
      code:load_file(?MODULE),
      Pid ! {reply, Tag, upgraded},
      store:running(Resources)

  end.

stop() -> utils:call_registered(?NAME, stop).

put(Resources) -> utils:call_registered(?NAME, {put, Resources}).

get() -> utils:call_registered(?NAME, get).

upgrade() -> utils:call_registered(?NAME, upgrade).

