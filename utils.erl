-module(utils).
-export([call/2, call/3, call_registered/2, call_registered/3]).

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
    undefined -> {error, no_server};
    Pid -> utils:call(Pid, Request, Timeout)
  end.

