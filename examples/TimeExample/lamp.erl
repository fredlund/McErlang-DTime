-module(lamp).
-compile(export_all).

-include("mce_opts.hrl").
-include("monState.hrl").
-include("state.hrl").

sim() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      discrete_time=true,
      is_infinitely_fast=true,
      sends_are_sefs=true,
      algorithm=mce_alg_simulation,
      sim_actions=true}).

mc() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=true,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}).

dot() ->
  mc(),
  file:write_file
    (atom_to_list(?MODULE)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun ({_,_SysMon}) ->
	  %%State = SysMon#monState.state,
	  %%io_lib:format("label=\"~.2f\"",[sectime(State#state.time)])
	  ""
      end,
      fun print_actions/1)).

mc2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}).

dot2() ->
  mc2(),
  file:write_file
    (atom_to_list(?MODULE)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun ({_,_SysMon}) ->
	  %%State = SysMon#monState.state,
	  %%io_lib:format("label=\"~.2f\"",[sectime(State#state.time)])
	  ""
      end,
      fun print_actions/1)).

debug() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      sends_are_sefs=true,
      algorithm=mce_alg_debugger,
      sim_actions=true}).

print_actions(Actions) ->
  "label=\""++
  lists:foldr
    (fun (Action,Rest) ->
	 if 
	   Rest=="" ->
	     io_lib:format("~s",[print_action(Action)]);
	   true ->
	     io_lib:format("~s,~s",[Rest,print_action(Action)])
	 end
     end, "", Actions)++
    "\"".

print_action(Action) ->
  case mce_erl_actions:is_probe(Action) of
    true ->
      io_lib:format("~p",[mce_erl_actions:get_probe_label(Action)]);
    false ->
      case mce_erl_actions:is_send(Action) of
	true -> 
	  io_lib:format("sent ~p",[mce_erl_actions:get_send_msg(Action)]);
	false ->
	  io_lib:format("~p",[mce_erl_actions:get_name(Action)])
      end
  end.

lampsystem() ->		 
  Lamp = spawn(?MODULE,lamp,[]),
  spawn(?MODULE,user,[Lamp]),
  mce_erl:urgent(mce_erl:pause(fun () -> ok end)).

user(Lamp) ->
  latest(1,2,5,fun () -> call(Lamp,press), user(Lamp) end).

latest(_Tick,0,F) -> F();
latest(Tick,Time,F) ->
  mce_erl:choice
    ([fun () -> F() end,
      fun () -> receive after Tick -> latest(Tick,Time-Tick,F) end end]).

latest(Tick,MinDelay,MaxDelay,F) ->
  receive
  after MinDelay -> latest(Tick,MaxDelay-MinDelay,F)
  end.

call(Pid,Msg) ->
  Pid!{call,Msg,self()},
  receive
    {reply,Value} ->
      Value
  end.

reply(Caller,Value) ->
  Caller!{reply,Value}.

lamp() ->
  receive
    {call,press,Caller1} ->
      reply(Caller1,ok),
      PressTime = mce_erl_time:nowRef(),
      mce_erl:probe(low),
      receive
	{call,press,Caller2} ->
	  reply(Caller2,ok),
	  case
	    compareTimes_ge
	    (mce_erl_time:now(),
	     addTimeStamps
	       (milliSecondsToTimeStamp(5),
		mce_erl_time:was(PressTime))) of
	    true ->
	      mce_erl:probe(off),
	      mce_erl_time:forget(PressTime),
	      lamp();
	    false ->
	      mce_erl:probe(bright),
	      mce_erl_time:forget(PressTime),
	      receive
		{call,press,Caller3} ->
		  reply(Caller3,ok),
		  mce_erl:probe(off),
		  lamp()
	      end
	  end
      end
  end.

milliSecondsToTimeStamp(MilliSeconds) ->
  Seconds = MilliSeconds div 1000,
  MegaSeconds = Seconds div 1000000,
  {MegaSeconds, Seconds rem 1000000, MilliSeconds rem 1000 * 1000}.

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

compareTimes_ge({M1, S1, Mic1}, {M2, S2, Mic2}) ->
  M1 > M2
    orelse M1 =:= M2 andalso S1 > S2
    orelse M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2.




		   
	       

