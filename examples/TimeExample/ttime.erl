-module(ttime).
-compile(export_all).

-include("mce_opts.hrl").

sim() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_simulation,
      sim_actions=true}).

mc() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true}).

debug() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_debugger,
      sim_actions=true}).

start() ->
  Reader = 
    spawn
      (fun () ->
	   receive
	     {X,Sender} ->
	       sleep(1000),
	       Sender!X
	   end
       end),
  Writer =
    spawn
      (fun () ->
	   sleep(500),
	   Reader!{hello,self()},
	   receive X -> X end,
	   sleep(1500)
       end).

sleep(Milliseconds) ->
  receive
  after Milliseconds -> ok
  end.

		 

		   
	       

