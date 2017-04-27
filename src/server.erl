-module(server).
-export([start/1, init/1]).
-export([stop/0, stop/1, allocate/0, allocate/1, deallocate/0, deallocate/1,
         inspect/0, inspect/1, deallocate_resource/1, deallocate_resource/2]).

-define(TIMEOUT, 1000).
-define(NAME, resources).


%% === Running a server ===

%% @doc Spawns the resource server.
start(StorePid) ->
  spawn(fun() -> init(StorePid) end).

%% @doc Starts the resource server.
init(StorePid) ->
  case utils:call(StorePid, get) of
    {ok, Resources} ->
      register(?NAME, self()),
      process_flag(trap_exit, true),
      log:info(?NAME, "starting with allocations: ~p~n", [Resources]),
      running(StorePid, Resources);
    Msg -> log:warn(?NAME, "could not contact store: ~p~n", [Msg])
  end.

%% @private
running(StorePid, Resources) ->

  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, allocate} ->
      {NewResources, Reply} = allocate(Resources, Pid),
      utils:call(StorePid, {put, NewResources}),
      Pid ! {reply, Tag, Reply},
      running(StorePid, NewResources);

    {request, Tag, Pid, deallocate} ->
      {NewResources, Reply} = deallocate(Resources, Pid),
      utils:call(StorePid, {put, NewResources}),
      Pid ! {reply, Tag, Reply},
      running(StorePid, NewResources);

    {request, Tag, Pid, {deallocate, Res}} ->
      NewResources = try deallocate(Resources, Pid, Res) of
          {NewResources_, Reply} ->
            utils:call(StorePid, {put, NewResources_}),
            Pid ! {reply, Tag, Reply},
            NewResources_
      catch
        throw:resource_not_allocated ->
          Pid ! {reply, Tag, {error, resource_not_allocated}},
          Resources;
        throw:resource_not_owned ->
          Pid ! {reply, Tag, {error, resource_not_owned}},
          Resources
      end,
      %% Recursive call should be outside try-catch block to benefit from
      %% tail-call optimization.
      running(StorePid, NewResources);

    {request, Tag, Pid, inspect} ->
      Pid ! {reply, Tag, Resources},
      running(StorePid, Resources);

    {request, Tag, Pid, {set_store, NewStorePid}} ->
      Pid ! {reply, Tag, ok},
      utils:call(NewStorePid, {put, Resources}),
      running(NewStorePid, Resources);

    {'EXIT', Pid, _Reason} ->
      {NewResources, _Reply} = deallocate(Resources, Pid),
      utils:call(StorePid, {put, NewResources}),
      running(StorePid, NewResources);

    _ -> throw(unknown_message)

  end.


%% === Synchronous functions to interact with the server ===

%% @doc Allocate a resource for the calling process.
allocate() -> allocate(?TIMEOUT).
allocate(Timeout) -> call_server(allocate, Timeout).

%% @doc Deallocate the resource held by the calling process.
deallocate() -> deallocate(?TIMEOUT).
deallocate(Timeout) -> call_server(deallocate, Timeout).

deallocate_resource(Res) -> deallocate_resource(Res, ?TIMEOUT).
deallocate_resource(Res, Timeout) ->
  call_server({deallocate, Res}, Timeout).

%% @doc Stop the server.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> call_server(stop, Timeout).

inspect() -> inspect(?TIMEOUT).
inspect(Timeout) -> call_server(inspect, Timeout).

%% @private
call_server(Request, Timeout) ->
  utils:call_registered(?NAME, Request, Timeout).

%% === Allocating and deallocating resources ===

%% @private
allocate({Free, Taken}=Resources, Pid) ->
  case maps:get(Pid, Taken, none) of
    none ->
      case Free of
        [] ->
          {Resources, {error, no_resource}};
        [Res|More] ->
          link(Pid),
          {{More, maps:put(Pid, Res, Taken)}, {allocated, Res}}
      end;
    Res -> {Resources, {error, Res}}
  end.

%% @private
%% @doc Deallocate resource by owner
deallocate({Free, Taken}=Resources, Pid) ->
  case maps:take(Pid, Taken) of
    error ->
      {Resources, {error, no_resource}};
    {Res, NewTaken} ->
      unlink(Pid),
      {{[Res|Free], NewTaken}, {deallocated, Res}}
  end.

%% @private
%% @doc Deallocate by owner and resource; throws when there is no match.
deallocate({Free, Taken}, Pid, Res) ->
  case lists:keytake(Res, 2, maps:to_list(Taken)) of
    false -> throw(resource_not_allocated);
    {value, {Pid, Res}, NewTaken} ->
      {{[Res|Free], maps:from_list(NewTaken)}, {deallocated, Res}};
    {value, {_, Res}, _} ->
      throw(resource_not_owned)
  end.

