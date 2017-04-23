-module(server).
-export([start/0, start/1, init/0, init/1]).
-export([stop/0, stop/1, allocate/0, allocate/1, deallocate/0, deallocate/1,
         inspect/0, inspect/1]).

-define(TIMEOUT, 1000).
-define(NAME, resources).


%% === Running a server ===

%% @doc Spawns the resource server.
start() -> start(resources()).
start(Resources) ->
  spawn(fun() -> init(Resources) end).

%% @doc Starts the resource server.
init() -> init(resources()).
init(Resources) ->
  register(?NAME, self()),
  process_flag(trap_exit, true),
  running({Resources, #{}}).

%% @private
running(Resources) ->
  receive

    {request, Tag, Pid, stop} ->
      Pid ! {reply, Tag, stopped};

    {request, Tag, Pid, allocate} ->
      {NewResources, Reply} = allocate(Resources, Pid),
      Pid ! {reply, Tag, Reply},
      running(NewResources);

    {request, Tag, Pid, deallocate} ->
      {NewResources, Reply} = deallocate(Resources, Pid),
      Pid ! {reply, Tag, Reply},
      running(NewResources);

    {request, Tag, Pid, inspect} ->
      Pid ! {reply, Tag, Resources},
      running(Resources);

    {'EXIT', Pid, _Reason} ->
      {NewResources, _Reply} = deallocate(Resources, Pid),
      running(NewResources)

  end.

%% @private
resources() -> lists:seq(10, 20).


%% === Synchronous functions to interact with the server ===

%% @doc Allocate a resource for the calling process.
allocate() -> allocate(?TIMEOUT).
allocate(Timeout) -> call_server(allocate, Timeout).

%% @doc Deallocate the resource held by the calling process.
deallocate() -> deallocate(?TIMEOUT).
deallocate(Timeout) -> call_server(deallocate, Timeout).

%% @doc Stop the server.
stop() -> stop(?TIMEOUT).
stop(Timeout) -> call_server(stop, Timeout).

inspect() -> inspect(?TIMEOUT).
inspect(Timeout) -> call_server(inspect, Timeout).

%% @private
call_server(Request, Timeout) ->
  case whereis(?NAME) of
    undefined -> {error, no_server};
    Pid -> utils:call(Pid, Request, Timeout)
  end.


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
deallocate({Free, Taken}=Resources, Pid) ->
  case maps:take(Pid, Taken) of
    error ->
      {Resources, {error, no_resource}};
    {Res, NewTaken} ->
      unlink(Pid),
      {{[Res|Free], NewTaken}, {deallocated, Res}}

  end.

