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
    (atom_to_list(hej)++".dot",
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
    (atom_to_list(hej)++".dot",
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

print_actions(Actions0) ->
  Actions =
    collapse_actions(Actions0),
  SourceStr =
    case Actions of
      [Action|_] ->
	io_lib:format
	  ("~p: ",[simplify_pids(mce_erl_actions:get_source(Action))]);
      _ ->
	""
    end,
  ActionsStr =
    lists:foldl
      (fun (Action,Output) ->
	   ActionStr = print_action(Action),
	   if 
	     ActionStr=="" ->
	       Output;
	     Output=="" ->
	       io_lib:format("~s",[ActionStr]);
	     Output==[""] ->
	       io_lib:format("~s",[ActionStr]);
	     true ->
	       io_lib:format("~s,~s",[Output,ActionStr])
	   end
       end, "", Actions),
  "label=\""++SourceStr++ActionsStr++"\"".

print_action(Action) ->
  case mce_erl_actions:is_probe(Action) of
    true ->
      io_lib:format("~p",[mce_erl_actions:get_probe_label(Action)]);
    false ->
      case mce_erl_actions:is_send(Action) of
	true -> 
	  io_lib:format
	    ("sent ~p",
	     [simplify_pids(mce_erl_actions:get_send_msg(Action))]);
	false ->
	  case mce_erl_actions:is_api_call(Action) of
	    true ->
	      io_lib:format
		("~p(~p) -> ~p",
		 [mce_erl_actions:get_api_call_fun(Action),
		  mce_erl_actions:get_api_call_arguments(Action),
		  mce_erl_actions:get_api_call_result(Action)]);
	    false ->
	      case mce_erl_actions:is_timeout(Action) of
		true ->
		  case mce_erl_actions:get_timeout(Action) of
		    Tick={_,_,_} ->
		      io_lib:format("timeout ~p",[Tick]);
		    _ ->
		      io_lib:format("timeout",[])
		  end;
		false ->
		  case mce_erl_actions:get_name(Action) of
		    run -> "";
		    Name -> io_lib:format("~p",[Name])
		  end
	      end
	  end
      end
  end.

collapse_actions(Actions) ->
  collapse_actions(Actions,[]).

collapse_actions([],Actions) ->
  lists:reverse(Actions);
collapse_actions([Action|Rest],Actions) ->
  case mce_erl_actions:is_choice(Action) orelse mce_erl_actions:is_run(Action) of
    true ->
      collapse_actions(Rest,Actions);    
    false ->
      case Actions of
	[] -> collapse_actions(Rest,[Action]);
	[Action2|Rest2] ->
	  case mce_erl_actions:is_timeout(Action) andalso mce_erl_actions:is_timeout(Action2) of
	    true ->
	      collapse_actions
		(Rest,
		 [mce_erl_actions:mk_timeout
		    (mce_erl_actions:get_source(Action),
		     addTimeStamps
		     (mce_erl_actions:get_timeout(Action),
		      mce_erl_actions:get_timeout(Action2)))|
		  Rest2]);
	    false ->
	      collapse_actions(Rest,[Action,Action2|Rest2])
	  end
      end
  end.

simplify_pids({pid,_,Pid}) ->
  Pid;
simplify_pids([First|Rest]) ->
  [simplify_pids(First)|simplify_pids(Rest)];
simplify_pids(Tuple) when is_tuple(Tuple) ->
  list_to_tuple(lists:map(fun simplify_pids/1,tuple_to_list(Tuple)));
simplify_pids(Other) -> 
  Other.

lampsystem() ->		 
  Lamp = spawn(?MODULE,lamp,[]),
  spawn(?MODULE,user,[Lamp]).

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
	  case
	    compareTimes_ge
	    (mce_erl_time:now(),
	     addTimeStamps
	       (milliSecondsToTimeStamp(5),
		mce_erl_time:was(PressTime))) of
	    true ->
	      mce_erl:probe(off),
	      mce_erl_time:forget(PressTime),
	      reply(Caller2,ok),
	      lamp();
	    false ->
	      mce_erl:probe(bright),
	      mce_erl_time:forget(PressTime),
	      reply(Caller2,ok),
	      receive
		{call,press,Caller3} ->
		  mce_erl:probe(off),
		  reply(Caller3,ok),
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




		   
	       

