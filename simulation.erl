-module(simulation).
-export([retry_and_stop/0, retry_and_spin/0, toggle/0]).
-export([scenario_stop_client/1, scenario_stop_server/1]).


%% === Specific behaviours ===

%% @doc Attempts to allocate a single resource continuously, until it
%% succeeds and stops.
retry_and_stop() -> {fun(initial) ->
                         case server:allocate() of
                           {error, no_server} -> initial;
                           {error, no_resource} -> initial;
                           {_, _} -> final
                         end;
                        (final) -> final
                     end, 0,
                     fun(initial) -> false; (final) -> true end,
                     initial}.

retry_and_spin() -> {fun(initial) ->
                         case server:allocate() of
                           {error, no_server} -> initial;
                           {error, no_resource} -> initial;
                           {_, _} -> final
                         end;
                        (final) -> final
                     end, 0,
                     fun(_) -> false end,
                     initial}.

toggle() -> {fun(empty) ->
                 case server:allocate() of
                   {error, no_server} -> empty;
                   {error, no_resource} -> empty;
                   {_, _} -> full
                 end;
                (full) ->
                 case server:deallocate() of
                   {error, no_server} -> full;
                   _ -> empty
                 end
             end, 0,
             fun(_) -> false end,
             empty}.

scenario_stop_client(Tick) ->
  case whereis(resources) of
    undefined -> server:start();
    _ -> ok
  end,
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  C = client:start(client:debug("client", Tick, retry_and_spin())),
  timer:sleep(2 * Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  client:stop(C),
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  server:stop().


scenario_stop_server(Tick) ->
  case whereis(resources) of
    undefined -> server:start();
    _ -> ok
  end,
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  C = client:start(client:debug("client", Tick, retry_and_spin())),
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  server:stop(),
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  client:stop(C).

scenario_stop_supervised_server(Tick) ->
  case whereis(supervisor) of
    undefined -> server:start();
    _ -> ok
  end,
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  C = client:start(client:debug("client", Tick, retry_and_spin())),
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  server:stop(),
  timer:sleep(Tick),
  io:format("resources: ~p~n", [server:inspect()]),
  timer:sleep(Tick),
  client:stop(C).

