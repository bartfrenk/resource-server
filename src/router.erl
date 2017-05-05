-module(router).

-export([start/2, init/2]).
-export([allocate/0, allocate/1, deallocate/1, deallocate/2,
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

    %% TODO: It is possible for a single process to allocate as many resources
    %% as there are servers. This seems like a difficult issue to avoid.
    {request, Tag, Pid, allocate} ->
      {NewLast, Reply} = allocate_with(Table, Pid, Last, next(Last, N)),
      Pid ! {reply, Tag, Reply},
      running(Table, NewLast, ResourceMap);

    {request, Tag, Pid, {deallocate, Res}} ->
      deallocate(Tag, Pid, Res, Table, ResourceMap),
      running(Table, Last, ResourceMap)
  end.

next(Index, N) -> Index rem N + 1.


allocate_with(_, _, Last, Last) -> {Last, error};
allocate_with(Table, Pid, Last, Next) ->
  N = length(Table),
  {_, ServerPid} = lists:nth(Next, Table),
  case utils:call(ServerPid, {allocate_for, Pid}) of
    {error, _} -> allocate_with(Table, Pid, Last, next(Next, N));
    Reply -> {Next, Reply}
  end.


find_server(Res, Table, ResourceMap) ->
  M = maps:filter(fun(_, {Rs, _}) -> lists:member(Res, Rs) end, ResourceMap),
  case maps:to_list(M) of
    [{I, _}] ->
      case lists:keyfind(I, 1, Table) of
        false -> none;
        {_, Pid} -> Pid
      end;
    _ -> none
  end.

deallocate(Tag, Pid, Res, Table, ResourceMap) ->
  case find_server(Res, Table, ResourceMap) of
    none ->
      log:debug(?NAME, "~s~n", [ResourceMap]),
      Pid ! {reply, Tag, error};
    ServerPid ->
      utils:forward(ServerPid, Tag, Pid, {deallocate, Res}, ?TIMEOUT)
  end.

%% @doc Allocate a resource for the calling process.
allocate() -> allocate(?TIMEOUT).
allocate(Timeout) ->
  utils:call_registered(?NAME, allocate, Timeout).

deallocate(Res) -> deallocate(Res, ?TIMEOUT).
deallocate(Res, Timeout) ->
  utils:call_registered(?NAME, {deallocate, Res}, Timeout).

%% @doc Stop the server.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> utils:call_registered(?NAME, stop, Timeout).


