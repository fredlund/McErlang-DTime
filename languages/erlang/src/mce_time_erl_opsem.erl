-module(mce_time_erl_opsem).

-export([initialState/2,initialState/3,transitions/2,commit/2,commit/3]).

-include("state.hrl").
-include("node.hrl").
-include("process.hrl").
-include("executable.hrl").

%%-define(debug,true).
-include("macros.hrl").
-include("emacros.hrl").

initialState(Program,Conf) ->
  initialState(list_to_atom("node0@" ++ net_adm:localhost()), Program, Conf).

initialState(NodeName, Expr, Conf) ->
  ?LOG("Booting system using node ~p and application ~p~n", [NodeName, Expr]),
  case mce_conf:is_infinitely_fast(Conf) of
    true ->
      InitialProcesses =
	case mce_conf:wants_rpc(Conf) of
	  true ->
	    [mce_erl_process:makeRunnable(Expr,NodeName,Conf),
	     mce_erl_process:makeRunnable({mce_erl_rpc,start,[]},NodeName,Conf)];
	  _ ->
	    [mce_erl_process:makeRunnable(Expr,NodeName,Conf)]
	end,
      #state{time={0,0,0},nodes=[#node{name=NodeName,processes=InitialProcesses}]};
    false ->
      io:format
	("*** Error: the field is_infinitely_fast must be set when using "++
	   "semantics ~p~n",
	 [?MODULE]),
      throw(?MODULE)
  end.

transitions(State, Conf) ->
  mce_erl_opsem:transitions(State,Conf).

commit(Alternative, Monitor, Conf) ->
  io:format("Alternative ~p chosen~n",[Alternative]),
  NewAlternative =
    case Alternative of
      {exec, Exec, State} ->
	Process = Exec#executable.process,
	case Process#process.status of
	  {timer,Deadline} ->
	    NewState =
	      State#state{time=addTimeStamps(State#state.time,Deadline)},
	    {exec,Exec,NewState};
	  _ ->
	    Alternative
	end;
      _ -> Alternative
    end,
  put(mc_monitor, Monitor),
  mce_erl_opsem:doStep(NewAlternative, Conf).

addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

commit(Alternative, Conf) ->
  commit(Alternative, void, Conf).

adjust_time(void,Process) ->
  Process;
adjust_time(Now,Process) ->
  case Process#process.status of
    {timer,Deadline} ->
      io:format("Now is ~p Old deadline was ~p~n",[Now,Deadline]),
      NewDeadline = minusTimeStamps(Deadline,Now),
      io:format("New deadline is ~p~n",[NewDeadline]),
      Process#process{status={timer,NewDeadline}},
      Process;
    _ ->
      Process
  end.

minusTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  {Mic,NewS1,NewM1} =
    if
      Mic1>=Mic2 -> {Mic1-Mic2,S1,M1};
      S1>0 -> {(Mic1-Mic2)+1000000,S1-1,M1};
      M1>0 -> {(Mic1-Mic2)+1000000,S1+1000000-1,M1-1}
  end,
  {Sec,NewNewM1} =
    if
      NewS1>=S2 -> {NewS1-S2,NewM1};
      NewM1>0 -> {(NewS1-S2)+1000000,NewM1-1}
    end,
  {NewNewM1-M2,Sec,Mic}.


