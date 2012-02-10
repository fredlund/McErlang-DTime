-module(door).
-export([start/1, button/2, init/1]).
-export([locked/3, open/3, open/2]).
-export([run/3,debug/3,debug2/3,mc/3,mc2/3,dot/3,dot2/3]).

-include("mce_opts.hrl").
-behaviour(gen_fsm).

start(Code) -> 
  gen_fsm:start(door, Code, []).

init(Code) -> 
  {ok, locked, Code}.

button(Pid,Password) -> 
  gen_fsm:sync_send_event(Pid, {button, Password}).

locked({button, Password}, _, Code) -> 
  case Password of
    Code -> 
      mce_erl:probe(unlock),
      {reply, ok, open, Code, 10000};
    Wrong -> 
      mce_erl:probe(wrong),
      {reply, ok, locked, Code}
  end.

open({button, Password}, _, Code) -> 
  mce_erl:probe(lock),
  {reply, ok, locked, Code}.

open(timeout, Code) -> 
  mce_erl:probe(lock),
  {next_state, locked, Code}.


%% ======================================================================

run(N,Tick,MaxWait) ->
  {ok,Pid} = start(2),
  spawn(fun () -> user(Pid,N,Tick,MaxWait) end).

user(Pid,N,Tick,MaxWait) -> 
  latest
    (Tick,
     MaxWait,
     fun () -> button(Pid,random:uniform(N)) end),
  user(Pid,N,Tick,MaxWait).
  
latest(_Tick,0,{M,F,Args}) ->
  mce_erl:urgent(), 
  apply(M,F,Args);
latest(_Tick,0,Fun) ->
  mce_erl:urgent(), 
  apply(Fun,[]);
latest(Tick,Time,Fun={M,F,Args}) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), apply(M,F,Args) end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,Fun) end end]);
latest(Tick,Time,Fun) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), apply(Fun,[]) end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,Fun) end end]).

%% ======================================================================

mc(N,Tick,MaxWait) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={?MODULE,run,[N,Tick,MaxWait]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      shortest=true,
      save_table=true,
      discrete_time=true}).

dot(N,Tick,MaxWait) ->
  mc(N,Tick,MaxWait),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

mc2(N,Tick,MaxWait) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={?MODULE,run,[N,Tick,MaxWait]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      shortest=true,
      well_behaved=true,
      partial_order=true,
      save_table=true,
      discrete_time=true}).

dot2(N,Tick,MaxWait) ->
  mc2(N,Tick,MaxWait),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

print_states() ->
  Result = mce:result(),
  Table = mce_result:table(Result),
  {ok, States} = mce_behav_tableOps:states_to_list(Table),
  lists:foreach(fun (State) -> io:format("~p~n",[State]) end, States).

print_actions(Actions0) ->
  Actions = collapse_actions(Actions0),
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
	    ("sent ~p to ~p",
	     [simplify_msg(simplify_pids(mce_erl_actions:get_send_msg(Action))),
	      simplify_pids(mce_erl_actions:get_send_pid(Action))]);
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

simplify_pids({pid,_,Pid}) ->
  Pid;
simplify_pids([First|Rest]) ->
  [simplify_pids(First)|simplify_pids(Rest)];
simplify_pids(Tuple) when is_tuple(Tuple) ->
  list_to_tuple(lists:map(fun simplify_pids/1,tuple_to_list(Tuple)));
simplify_pids(Other) -> 
  Other.

simplify_msg({sync_send_event,Event,_}) ->
  Event;
simplify_msg({reply,_,Reply}) ->
  Reply;
simplify_msg(Msg) ->  
  Msg.

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

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

debug(N,Tick,MaxWait) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={?MODULE,run,[N,Tick,MaxWait]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}).

debug2(N,Tick,MaxWait) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={?MODULE,run,[N,Tick,MaxWait]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      well_behaved=true,
      partial_order=true,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}).
