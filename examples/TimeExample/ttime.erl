-module(ttime).
-compile(export_all).

-include("mce_opts.hrl").
-include("monState.hrl").
-include("state.hrl").

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
      table=mce_table_hashWithActions,
      save_table=true,
      discrete_time=true}).

mcdot() ->
  mc(),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun ({_,SysMon}) ->
	  State = SysMon#monState.state,
	  %%io_lib:format("label=\"~.2f\"",[sectime(State#state.time)])
	  ""
      end,
      fun print_actions/1)).

print_actions(Actions) ->
  "label="++
  lists:foldr
    (fun (Action,Rest) ->
	 if 
	   Rest=="" ->
	     io_lib:format("~p",[mce_erl_actions:get_name(Action)]);
	   true ->
	     io_lib:format("~s,~p",[Rest,mce_erl_actions:get_name(Action)])
	 end
     end, "", Actions).
      
sectime({_,Seconds,MicroSeconds}) ->
  Seconds + (MicroSeconds/1000000).

debug() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_debugger,
      sim_actions=true}).

start() ->
  Reader = spawn(?MODULE,reader,[]),
  Writer = spawn(?MODULE,writer,[Reader]).

reader() ->
  receive
    {X,Sender} ->
      sleep(1000),
      Sender!X,
      reader()
  end.

writer(Reader) ->
  delay(100,1000),
  Reader!{hello,self()},
  receive X -> X end,
  sleep(1500),
  writer(Reader).

delay(_Tick,Max) when Max=<0 ->
  ok;
delay(Tick,Max) ->
  mce_erl:choice
    ([fun () -> ok end,
      fun () -> receive after Tick -> delay(Tick,Max-Tick) end end]).

sleep(Milliseconds) ->
  receive
  after Milliseconds -> ok
  end.

		 

		   
	       

