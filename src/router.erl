-module(router).

-export([start/2, init/2]).
-export([allocate/0, allocate/1, deallocate_resource/1, deallocate_resource/2,
         stop/0, stop/1]).

-define(TIMEOUT, 1000).
-define(NAME, router).


start(StorePid, ServerPids) ->
  spawn(fun() -> init(StorePid, ServerPids) end).

init(StorePid, ServerPids) ->
  register(?NAME, self()), %% easier to refer to router
  log:info(?NAME, "contacting store ~p~n", [StorePid]),
  case utils:call(StorePid, get) of
    {ok, ResourceMap} ->
      Table = lists:zip(lists:sort(maps:keys(ResourceMap)), ServerPids),
      log:info(?NAME, "created routing table ~p~n", [Table]),
      running(Table, length(Table), ResourceMap);
    Msg -> log:warn(?NAME, "could not contact store: ~p~n", [Msg])
  end.

running(Table, Last, ResourceMap) ->
  N = length(Table),
  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, allocate} ->
      {NewLast, Reply} = allocate_with(Table, Last, next(Last, N)),
      Pid ! {reply, Tag, Reply},
      running(Table, NewLast, ResourceMap)
  end.

next(Index, N) -> Index rem N + 1.


allocate_with(_, Last, Last) -> {Last, error};
allocate_with(Table, Last, Next) ->
  N = length(Table),
  {_, Pid} = lists:nth(Next, Table),
  case utils:call(Pid, allocate) of
    {error, _} -> allocate_with(Table, Last, next(Next, N));
    Reply -> {Next, Reply}
  end.

%% @doc Allocate a resource for the calling process.
allocate() -> allocate(?TIMEOUT).
allocate(Timeout) ->
  utils:call_registered(?NAME, allocate, Timeout).

deallocate_resource(Res) -> deallocate_resource(Res, ?TIMEOUT).
deallocate_resource(Res, Timeout) ->
  utils:call_registered(?NAME, {deallocate, Res}, Timeout).

%% @doc Stop the server.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> utils:call_registered(?NAME, stop, Timeout).


