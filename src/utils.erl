-module(utils).
-export([call/2, call/3, forward/5, call_registered/2, call_registered/3]).
-export([randoms/3]).

-define(TIMEOUT, 1000).

%% @doc Might block indefinitely due to absence of timeout.
call(Client, Request) -> call(Client, Request, ?TIMEOUT).

call(Client, Request, Timeout) ->
  Tag = make_ref(),
  Client ! {request, Tag, self(), Request},
  receive
    {reply, Tag, Reply} -> Reply
  after Timeout -> timeout
  end.

call_registered(Name, Request) -> call_registered(Name, Request, ?TIMEOUT).

call_registered(Name, Request, Timeout) ->
  case whereis(Name) of
    undefined -> {error, no_process};
    Pid -> call(Pid, Request, Timeout)
  end.

%% @doc Generate a list of N randoms between Lo and Hi.
randoms(N, Lo, Hi) ->
  Gen = fun(_) -> rand:uniform(Hi - Lo + 1) + (Lo - 1) end,
  lists:map(Gen, lists:seq(1, N)).


forward(Client, Tag, Sender, Request, Timeout) ->
  Client ! {request, Tag, Sender, Request},
  receive
    {reply, Tag, Reply} -> Reply
  after Timeout -> timeout
  end.

