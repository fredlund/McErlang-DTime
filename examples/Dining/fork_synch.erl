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
	fun (Philosopher) -> 
	    mce_erl:synch
	      ([{out,
		 {{fork,Philosopher},
		  fun () -> true end,
		  ok,
		  fun () -> taken()
		  end}}])
	end}}]).

taken() ->
  mce_erl:synch
    ([{in,
       {{release,self()},
	fun (X) -> true end,
	fun (Philosopher) -> free() end}}]).

