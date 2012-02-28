-module(test1).
-compile(export_all).

-include("mce_opts.hrl").
-include("monState.hrl").
-include("state.hrl").

sim() ->
  mce:start
    (#mce_opts
     {program={?MODULE,lampsystem,[]},
      %%discrete_time=true,
      %%is_infinitely_fast=true,
      sends_are_sefs=true,
      algorithm=mce_alg_simulation,
      sim_actions=true}).

mc() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      %%is_infinitely_fast=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}).

dot() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      %%is_infinitely_fast=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot1)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun no_state_time/1,
      fun print_actions/1)).

mc2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      %%is_infinitely_fast=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}).

mc3() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}).

dot4() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system1,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      abstraction=mce_abs_norm_notime,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot4)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun state_time/1,
      fun print_actions/1)).
  
dot5() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system2,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      abstraction=mce_abs_norm_notime,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot5)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun state_time/1,
      fun print_actions/1)).

debug5() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system2,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      abstraction=mce_abs_norm_notime,
      well_behaved=true,
      algorithm=mce_alg_debugger,
      partial_order=true,
      sends_are_sefs=true,
      sim_actions=true,
      %%discrete_time=true,
      save_table=true}).

dot6() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system2,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      %%abstraction=mce_abs_norm_notime,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot6)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun no_state_time/1,
      fun print_actions/1)).

dot7() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system3,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      %%abstraction=mce_abs_norm_notime,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot6)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun no_state_time/1,
      fun print_actions/1)).
  
debug6() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system2,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=false,
      %%abstraction=mce_abs_norm_notime,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}).

clock(Tick,0) -> ok;
clock(Tick,N) when N>0 ->
  mce_erl:urgent(),
  receive
  after Tick -> clock(Tick,N-Tick)
  end.

clock(Tick) ->
  mce_erl:urgent(),
  receive
  after Tick -> clock(Tick)
  end.

dot2() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      %%is_infinitely_fast=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot2)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun no_state_time/1,
      fun print_actions/1)).

state_void(_) ->
  "".

state_time({_,SysMon}) ->
  State = SysMon#monState.state,
  case mce_erl:has_probe_state(gotIt,State) of
    true ->
      io_lib:format("fillcolor=green,style=filled,label=\"~p\"",[to_time(State#state.time)]);
    false ->
      case mce_erl:has_probe_state(timedOut,State) of
	true ->
	  io_lib:format("fillcolor=red,style=filled,label=\"~p\"",[to_time(State#state.time)]);
	false -> 
	  io_lib:format("label=\"~p\"",[to_time(State#state.time)])
      end
  end.

no_state_time({_,SysMon}) ->
  State = SysMon#monState.state,
  case mce_erl:has_probe_state(gotIt,State) of
    true ->
      io_lib:format("fillcolor=green,style=filled",[]);
    false ->
      case mce_erl:has_probe_state(timedOut,State) of
	true ->
	  io_lib:format("fillcolor=red,style=filled",[]);
	false -> 
	  io_lib:format("",[])
      end
  end.

dot3() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      table=mce_table_hashWithActions,
      is_infinitely_fast=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      %%discrete_time=true,
      save_table=true}),
  file:write_file
    (atom_to_list(dot3)++".dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      fun no_state_time/1,
      fun print_actions/1)).

debug() ->
  mce:start
    (#mce_opts
     {program={?MODULE,system,[]},
      %%is_infinitely_fast=true,
      %%discrete_time=true,
      sends_are_sefs=true,
      algorithm=mce_alg_debugger,
      sim_actions=true}).

print_actions(Actions0) ->
  Actions =
    collapse_actions(Actions0),
%%  SourceStr =
%%    case Actions of
%%      [Action|_] ->
%%	io_lib:format
%%	  ("~p: ",[simplify_pids(mce_erl_actions:get_source(Action))]);
%%      _ ->
%%	""
%%    end,
  SourceStr = "",
  TimeOutStr =
    case lists:any
      (fun (Action) -> 
	   mce_erl:is_probe(Action) andalso
	     mce_erl:probe_label(Action)==timedOut
       end, Actions0) of
      true -> "color=blue,penwidth=5,";
      false -> ""
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
  TimeOutStr++"label=\""++SourceStr++ActionsStr++"\"".

print_action(Action) ->
  case mce_erl_actions:is_probe(Action) of
    true ->
      "";
      %%io_lib:format("~p",[mce_erl_actions:get_probe_label(Action)]);
    false ->
      case mce_erl_actions:is_send(Action) of
	true -> 
	  io_lib:format
	    ("sent ~p",
	     [simplify_pids(mce_erl_actions:get_send_msg(Action))]);
	false ->
	  case mce_erl_actions:is_api_call(Action) of
	    true ->
%%	      io_lib:format
%%		("~p(~p) -> ~p",
%%		 [mce_erl_actions:get_api_call_fun(Action),
%%		  mce_erl_actions:get_api_call_arguments(Action),
%%		  mce_erl_actions:get_api_call_result(Action)]);
	      "";
	    false ->
	      case mce_erl_actions:is_timeout(Action) of
		true ->
		  case mce_erl_actions:get_timeout(Action) of
		    Tick={_,_,_} ->
		      io_lib:format("timeout ~p",[to_time(Tick)]);
		    _ ->
		      io_lib:format("timeout",[])
		  end;
		false ->
		  case mce_erl_actions:get_name(Action) of
		    run -> "";
		    spawn -> "";
		    recv -> "";
		    died -> "";
		    Name -> io_lib:format("~p",[Name])
		  end
	      end
	  end
      end
  end.

to_time({0,Seconds,Microseconds}) ->
  MilliSeconds=Seconds*1000+(Microseconds div 1000).

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

system() ->		 
P1 = spawn(fun () -> 
             receive
               Msg -> mce_erl:probe_state(gotIt)
             after 1000 -> mce_erl:probe(timedOut), mce_erl:probe_state(timedOut)
             end
           end),
spawn(fun () -> P1!hello end).

system1() ->
P1 = spawn(fun () -> 
             mce_erl:urgent(1500),
             receive
               Msg -> mce_erl:probe_state(gotIt)
             after 1000 -> mce_erl:probe(timedOut), mce_erl:probe_state(timedOut)
             end
           end),
spawn(fun () -> P1!hello end).

system2() ->
  spawn(fun () -> clock(500,2500) end),
P1 = spawn(fun () -> 
             mce_erl:urgent(1500),
             receive
               Msg -> mce_erl:probe_state(gotIt)
             after 1000 -> mce_erl:probe(timedOut), mce_erl:probe_state(timedOut)
             end
           end),
  spawn(fun () -> P1!hello end).

system3() ->
  spawn(fun () -> clock(500) end),
P1 = spawn(fun () -> 
             mce_erl:urgent(1500),
             receive
               Msg -> mce_erl:probe_state(gotIt)
             after 1000 -> mce_erl:probe(timedOut), mce_erl:probe_state(timedOut)
             end
           end),
  spawn(fun () -> P1!hello end).

latest(_Tick,0,F) -> F();
latest(Tick,Time,F) ->
  mce_erl:choice
    ([fun () -> F() end,
      fun () -> receive after Tick -> latest(Tick,Time-Tick,F) end end]).

latest(Tick,MinDelay,MaxDelay,F) ->
  receive
  after MinDelay -> latest(Tick,MaxDelay-MinDelay,F)
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


%% ======================================================================

all_dots() ->
  dot(),
  os:cmd("dot -Tpdf < dot1.dot > dot1.pdf"),
  dot2(),
  os:cmd("dot -Tpdf < dot2.dot > dot2.pdf"),
  dot3(),
  os:cmd("dot -Tpdf < dot3.dot > dot3.pdf"),
  dot4(),
  os:cmd("dot -Tpdf < dot4.dot > dot4.pdf"),
  dot5(),
  os:cmd("dot -Tpdf < dot5.dot > dot5.pdf"),
  dot6(),
  os:cmd("dot -Tpdf < dot6.dot > dot6.pdf"),
  dot7(),
  os:cmd("dot -Tpdf < dot6.dot > dot7.pdf").
  


		   
	       

