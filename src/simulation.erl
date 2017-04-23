-module(simulation).
-export([retry_and_stop/0, retry_and_spin/0, toggle/0]).
-export([scenario_single_client/1,
         scenario_multiple_clients/2]).


%% === Specific behaviours ===

%% @doc Attempts to allocate a single resource continuously, until it
%% succeeds and stops.
retry_and_stop() -> {fun(initial) ->
                         case server:allocate() of
                           {error, no_process} -> initial;
                           {error, no_resource} -> initial;
                           {_, _} -> final
                         end;
                        (final) -> final
                     end, 0,
                     fun(initial) -> false; (final) -> true end,
                     initial}.

retry_and_spin() -> {fun(initial) ->
                         case server:allocate() of
                           {error, no_process} -> initial;
                           {error, no_resource} -> initial;
                           {_, _} -> final
                         end;
                        (final) -> final
                     end, 0,
                     fun(_) -> false end,
                     initial}.

toggle() -> {fun(empty) ->
                 case server:allocate() of
                   {error, no_proces} -> empty;
                   {error, no_resource} -> empty;
                   {_, _} -> full
                 end;
                (full) ->
                 case server:deallocate() of
                   {error, no_process} -> full;
                   _ -> empty
                 end
             end, 0,
             fun(_) -> false end,
             empty}.

scenario_single_client(Tick) ->
  case whereis(overseer) of
    undefined -> overseer:start();
    _ -> ok
  end,
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  C = client:start(client:debug("client", Tick, retry_and_spin())),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  server:stop(),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  store:stop(),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  client:stop(C),
  overseer:stop(),
  server:stop(),
  store:stop(),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]).


client_name(I) -> "C" ++ integer_to_list(I).

scenario_multiple_clients(Tick, N) ->
  case whereis(overseer) of
    undefined -> overseer:start();
    _ -> ok
  end,
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  Delays = utils:randoms(N, Tick div 2, Tick + Tick div 2),
  Cs = lists:zipwith(
         fun(I, D) ->
             client:start(client:debug(client_name(I), D, toggle())) end,
         lists:seq(1, N), Delays),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  server:stop(),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  timer:sleep(Tick),
  store:stop(),
  timer:sleep(Tick),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]),
  lists:foreach(fun(C) -> client:stop(C) end, Cs),
  overseer:stop(),
  server:stop(),
  store:stop(),
  log:info(resources, "~p~n", [server:inspect()]),
  log:info(store, "~p~n", [store:get()]).
