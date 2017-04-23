-module(log).

-export([debug/3, info/3]).

debug(Src, Fmt, Args) ->
  io:format(Src ++ ": " ++ Fmt, Args).

info(Src, Fmt, Args) ->
  io:format(Src ++ ": " ++ Fmt, Args).
