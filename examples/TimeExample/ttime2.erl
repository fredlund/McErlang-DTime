-module(ttime2).
-compile(export_all).

-include("mce_opts.hrl").
-include("monState.hrl").
-include("state.hrl").

sim1() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_simulation,
      sim_actions=true}).

mc1() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      save_table=true,
      discrete_time=true}).

mcdot1() ->
  mc1(),
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

debug1() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_debugger,
      sim_actions=true}).

sim2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      is_infinitely_fast=true,
      discrete_time=true,
      algorithm=mce_alg_simulation,
      sim_actions=true}).

mc2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      save_table=true,
      discrete_time=true}).

mcdot2() ->
  mc2(),
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

debug2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      is_infinitely_fast=true,
      discrete_time=true,
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

sectime({_,Seconds,MicroSeconds}) ->
  Seconds + (MicroSeconds/1000000).

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

lampsystem() ->		 
  Lamp = spawn(?MODULE,lamp,[]),
  User = spawn(?MODULE,user,[Lamp]).

user(Lamp) ->
  sleep(100),
  delay(1000,6000),
  Lamp!press,
  user(Lamp).

lamp() ->
  sleep(50),
  lamp1().

lamp1() ->
  receive
    press ->
      PressTime = mce_erl_time:nowRef(),
      mce_erl:probe(low),
      %%mce_erl:apply(io,format,["Press time is ~p~n",[PressTime]]),
      receive
	press ->
	  %%mce_erl:apply(io,format,["Now is ~p~n",[mce_erl_time:now()]]),
	  case
	    compareTimes_ge
	    (mce_erl_time:now(),
	     addTimeStamps(milliSecondsToTimeStamp(2000),mce_erl_time:was(PressTime))) of
	    true ->
	      mce_erl:probe(off),
	      mce_erl_time:forget(PressTime),
	      lamp1();
	    false ->
	      mce_erl:probe(bright),
	      mce_erl_time:forget(PressTime),
	      receive
		press ->
		  mce_erl:probe(off),
		  lamp1()
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




		   
	       

