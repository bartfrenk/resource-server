-module(utils).
-export([call/2, call/3]).

%% @doc Might block indefinitely due to absence of timeout.
call(Client, Request) ->
  Tag = make_ref(),
  Client ! {request, Tag, self(), Request},
  receive
    {reply, Tag, Reply} -> Reply
  end.

call(Client, Request, Timeout) ->
  Tag = make_ref(),
  Client ! {request, Tag, self(), Request},
  receive
    {reply, Tag, Reply} -> Reply
  after Timeout -> timeout
  end.
