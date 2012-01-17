-module(ttime).

-include("mce_opts.hrl").

-export([test/0]).
-export([start/0]).

test() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      language=mce_time_erl_opsem,
      is_infinitely_fast=true,
      algorithm=mce_alg_simulation,
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

		 

		   
	       

