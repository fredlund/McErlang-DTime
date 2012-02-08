-module(fork).

-export([start/0]).

start() ->
  Fork = spawn(fun() -> free() end),
  Fork.

free() ->
  receive
    {grab, Pid} ->
      Pid ! fork,
      taken()
  end.

taken() ->
  receive
    {release, Pid} -> free()
  end.

