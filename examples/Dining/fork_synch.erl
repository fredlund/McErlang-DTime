-module(fork_synch).

-export([start/0]).

start() ->
  Fork = spawn(fun() -> free() end),
  Fork.

free() ->
  mce_erl:synch
    ([{in,
       {{grab,self()},
	fun (X) -> true end,
	fun (Philosopher) -> taken() end}}]).

taken() ->
  mce_erl:synch
    ([{in,
       {{release,self()},
	fun (X) -> true end,
	fun (Philosopher) -> free() end}}]).

