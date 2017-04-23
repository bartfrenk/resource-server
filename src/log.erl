-module(log).

-export([debug/3, info/3, warn/3, err/3]).

debug(Src, Fmt, Args) -> write(debug, Src, Fmt, Args).

info(Src, Fmt, Args) -> write(info, Src, Fmt, Args).

warn(Src, Fmt, Args) -> write(warn, Src, Fmt, Args).

err(Src, Fmt, Args) -> write(error, Src, Fmt, Args).

write(_Level, Src, Fmt, Args) ->
  io:format("~s: " ++ Fmt, [Src|Args]).
