%% Copyright (c) 2009, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Lars-Ake Fredlund
%% @copyright 2006-2009 Lars-Ake Fredlund
%% @doc
%% @private

%% Runtime system (handling process coordination) among processes

-module(mce_erl_opsem).
-export([initialState/2,initialState/3,
	 transitions/2,commit/3,commit/4,transcommit/2,checkReceive/1,
	 doStep/3,updProcStatusFromQueue/1,record_action/1]).

-include("process.hrl").
-include("state.hrl").
-include("system.hrl").
-include("executable.hrl").
-include("node.hrl").
-include("mce_opts.hrl").

%%-define(debug,true).
-include("macros.hrl").
-include("emacros.hrl").




%%% Returns the initial state

initialState(Program,Conf) ->
  initialState(list_to_atom("node0@" ++ net_adm:localhost()), Program, Conf).

initialState(NodeName, Expr, Conf) ->
  ?LOG("Booting system using node ~p and application ~p~n", [NodeName, Expr]),
  InitialProcesses =
    case mce_conf:wants_rpc(Conf) of
      true ->
	[mce_erl_process:makeRunnable(Expr,NodeName,Conf),
	 mce_erl_process:makeRunnable({mce_erl_rpc,start,[]},NodeName,Conf)];
      _ ->
	[mce_erl_process:makeRunnable(Expr,NodeName,Conf)]
    end,
  Time =
    case mce_conf:discrete_time(Conf) of
      true -> {0,0,0};
      false -> void
    end,
  #state{time=Time, nodes =[#node{name=NodeName,processes=InitialProcesses}]}.

%%% Computes all the transitions of a state

transitions(State, Conf) ->
  {Type,Transitions} = int_transitions(State,Conf),
  case mce_conf:random(Conf) of
    true ->
      randomise(Transitions);
    _ ->
      Transitions
  end.

int_transitions(State,Conf) ->
  %% In simulation mode we may have gotten external i/o waiting
  %% to be read here, and communicated to simulated processes.
  case mce_conf:external_io_possible(Conf) of
    true ->
      checkOverflow(),
      IOResult = {_,_,CommState} = moveIO(unchanged, [], State),
      {io,IOResult,allPossibilities(CommState,Conf)};
    _ ->
      allPossibilities(State, Conf)
  end.

%%% Commits to a transition
commit(Alternative, State, Monitor, Conf) ->
  put(mc_monitor, Monitor),
  (?MODULE):doStep(Alternative, State, Conf).

commit(Alternative, State, Conf) ->
  commit(Alternative, State, void, Conf).

%% Combines transitions and commits
transcommit(State,Conf) ->
  lists:usort(transcommit_int(State,Conf)).
  
%% Self-sends??

filter_nonprogressing(Transitions,State,Conf) ->
  case filter_nonprogressing(Transitions,[],State,length(Transitions)-1,Transitions,Conf) of
    [] -> Transitions;
    Other -> Other
  end.

filter_nonprogressing([],_,State,NumTransitions,Transitions,Conf) -> [];
filter_nonprogressing([Transition|Rest],Seen,State,NumTransitions,Transitions,Conf) ->
  Check = Seen++Rest,
  case affected(Transition,Check,State,Conf) of
    all -> [Transition|filter_nonprogressing(Rest,[Transition|Seen],State,NumTransitions,Transitions,Conf)];
    {some,L} ->
      case not(occurs(L,Check,State,Conf)) of
	true ->
	  {_,NewState0} = doStep(Transition,State,Conf),
	  {_,NewState} = collapse_locals([],NewState0,Conf),
	  {_,NewTransitions} = int_transitions(NewState,Conf),
	  if
	    length(NewTransitions) > NumTransitions ->
	      [Transition|filter_nonprogressing(Rest,[Transition|Seen],State,NumTransitions,Transitions,Conf)];
	    true -> 
	      filter_nonprogressing(Rest,[Transition|Seen],State,NumTransitions,Transitions,Conf)
	  end;
	false -> [Transition|filter_nonprogressing(Rest,[Transition|Seen],State,NumTransitions,Transitions,Conf)]
      end
  end.

affected({{synch,Port,P,{ExecInFun,Value},Q,ExecOutFun,Others},Node,OtherNodes},Transitions,State,Conf) ->
  {some,[P,Q]};
affected({exec,Exec},Transitions,State,Conf) ->
  P = Exec#executable.process,
  Pid = P#process.pid,
  Innermost = find_innermost(P#process.expr),
  case Innermost of
    {?SENDTAG,{{mcerlang,nget,_},_}} ->
      {some,[Pid]};
    {?SENDTAG,{{mcerlang,send,_},{_,_,[Pid2|_]}}} ->
      Enabled = find_all_pid_transitions(Pid2,Transitions),
      case can_receive(Pid2,Enabled,State,Conf) of
	true -> {some,[Pid,Pid2]};
	false -> {some,[Pid]}
      end;
    {?CHOICETAG,_} ->
      {sone,[Pid]};
    _ ->
      case P#process.status of
	{timer,{0,0,0}} -> {some,[Pid]};
	_ -> all
      end
  end;
affected(_,_,_,_) ->
  all.

can_receive(Pid,[],State,Conf) -> false;
can_receive(Pid,[Transition|Rest],State,Conf) ->
  %%io:format("~p: can_receive:~n~p~n",[Pid,Transition]),
  {_,NewState} = doStep(Transition,State,Conf),
  %%io:format("next state is ~n~p~n~n",[NewState]),
  {_,Transitions} = int_transitions(NewState,Conf),
  %%io:format("length of transitions is ~p~n",[length(Transitions)]),
  lists:any
    (fun (Trans) ->
	 case Trans of
	   {exec,Exec} ->
	     P = Exec#executable.process,
	     %%io:format("trans pid is ~p~n",[P#process.pid]),
	     if
	       P#process.pid==Pid andalso P#process.status==receivable ->
		 %%io:format("Checking trans~n~p~n",[Trans]),
		 {_,NewState2} = doStep(Trans,NewState,Conf),
		 {_,Transitions2} = int_transitions(NewState2,Conf),
		 Result=find_all_pid_transitions(Pid,Transitions2),
		 %%io:format("Result is~n~p~n",[Result]),
		 Result=/=[];
	       true ->
		 false
	     end;
	   _ ->
	     %%io:format("transition is ~p~n",[Trans]),
	     false
	 end
     end, Transitions) orelse can_receive(Pid,Rest,State,Conf).

find_all_pid_transitions(Pid,Transitions) ->
  lists:filter
    (fun (Transition) ->
	 case Transition of
	   {{synch,Port,P,{ExecInFun,Value},Q,ExecOutFun,Others},Node,OtherNodes} ->
	     Pid==P#process.pid orelse Pid==Q#process.pid;
	   {exec,Exec} ->
	     Pid==((Exec#executable.process)#process.pid);
	   _ ->
	     false
	 end
     end, Transitions).

help_print_trans({{synch,Port,P,{ExecInFun,Value},Q,ExecOutFun,Others},Node,OtherNodes}) ->
  io_lib:format
    ("transition ~p",
     [{synch,P#process.pid,Q#process.pid,Port,Value}]).

help_print_transs([]) -> "";
help_print_transs([Transition]) ->
  help_print_trans(Transition);
help_print_transs([Transition|Rest]) ->
  help_print_trans(Transition)++"\n"++
    help_print_transs(Rest).

occurs(Pids,Transitions,State,Conf) -> 
  PidSet = sets:from_list(Pids),
  occurs1(PidSet,Transitions,Transitions,State,Conf).

occurs1(PidSet,[],Transitions,State,Conf) -> false;
occurs1(PidSet,[Transition|Rest],Transitions,State,Conf) ->
  case affected(Transition,Transitions,State,Conf) of
    all -> true;
    {some,L} -> 
      sets:size(sets:intersection(PidSet,sets:from_list(L)))>0
	orelse occurs1(PidSet,Rest,Transitions,State,Conf)
  end.

prioritize_choices(Transitions) ->
  Transitions0 = 
    lists:filter
      (fun (Transition) ->
	   case Transition of
	     {exec,Exec} ->
	       Process = Exec#executable.process,
	       case Process#process.status of
		 runnable ->
		   case Process#process.expr of
		     {?CONTEXTTAG,{_,[{?WASCHOICETAG,_}|_]}} ->
		       true;
		     _ ->
		       false
		   end;
		 _ -> false
	       end;
	     _ -> false
	   end
       end, Transitions),
  if
    Transitions0==[] ->
      Transitions;
    true ->
      Transitions0
  end.

timeout_separate(Now,Transitions,Conf) ->
  lists:foldl
    (fun (Transition,{NonTimeouts,SlowTimeouts,UrgentTimeouts}) ->
	 case Transition of
	   {exec,Exec} ->
	     P = Exec#executable.process,
	     MaxWait = 
	       case is_urgent(Now,P#process.expr,Conf) of
		 false ->
		   infinity;
		 {true,MW} ->
		   MW
	       end,
	     case P#process.status of
	       {timer,Deadline} ->
		 if 
		   MaxWait==Now ->
		     {NonTimeouts,SlowTimeouts,[Transition|UrgentTimeouts]};
		   true ->
		     {NonTimeouts,[Transition|SlowTimeouts],UrgentTimeouts}
		 end;
	       _ -> {[Transition|NonTimeouts],SlowTimeouts,UrgentTimeouts}
	     end;
	   _ -> {[Transition|NonTimeouts],SlowTimeouts,UrgentTimeouts}
	 end
     end, {[],[],[]}, Transitions).

transcommit_int(State,Conf) ->
  {Actions,NewState} =
    collapse_locals([],State,Conf),
  PartRestrict =
    mce_conf:well_behaved(Conf) andalso mce_conf:partial_order(Conf),
  {_Type,Transitions0} =
    int_transitions(NewState,Conf),
  Transitions2 =
    if PartRestrict ->
	Transitions1 = prioritize_choices(Transitions0),
	filter_nonprogressing(Transitions1,State,Conf);
       true -> 
	Transitions0
    end,
  {NonTimeTransitions,SlowTimeTransitions,UrgentTimeTransitions} =
    timeout_separate(State#state.time,Transitions2,Conf),
  
  UrgentResults =
    if
      UrgentTimeTransitions=/=[] ->
	{NewerActionsTmp,NewerStateTmp} =
	  lists:foldl
	    (fun (Transition,{_,NS}) -> commit(Transition,NS,Conf) end,
	     {[],NewState}, UrgentTimeTransitions),
	[collapse_locals(Actions++NewerActionsTmp,NewerStateTmp,Conf)];
      true ->
	[]
    end,

  OtherResults =
   lists:map
     (fun (Alternative) ->
	  IsTimeout =
	    isTimeoutTransition(Alternative),
	  {NewerActions,NewerState} =
 	   commit(Alternative,NewState,Conf),
 	 {NewerActions1,NewerState1} = 
 	   collapse_locals(Actions++NewerActions,NewerState,Conf),
	  if
	    IsTimeout ->
	      {_,Transitions3} = 
		int_transitions(NewerState1,Conf),
	      case filter_nonlocals_time(Transitions3,source(Alternative)) of
		[] ->
		  {NewerActions1,NewerState1};
		[OneTransition] -> 
		  {NewerActions2,NewerState2} =
		    commit(OneTransition,NewerState1,Conf),
		  collapse_locals(NewerActions1++NewerActions2,NewerState2,Conf);
		_ ->
		  {NewerActions1,NewerState1}
	      end;
	    true -> {NewerActions1,NewerState1}
	  end
      end, 
      SlowTimeTransitions++NonTimeTransitions),
  %%
  UrgentResults++OtherResults.

filter_nonlocals_time(Transitions,Source) ->
  lists:filter
    (fun (Transition) ->
	 isLocalTransition(Transition,Source) andalso
	   not(isTimeoutTransition(Transition))
     end, Transitions).

source(Transition) ->
  case Transition of
    {exec,Exec} ->
      P = Exec#executable.process,
      Pid = P#process.pid
  end.

isLocalTransition(Transition,Pid) ->
  case Transition of
    {exec,Exec} ->
      P = Exec#executable.process,
      P#process.pid == Pid;
    _ -> false
  end.

isTimeoutTransition(Transition) ->
  case Transition of
    {exec,Exec} ->
      P = Exec#executable.process,
      Pid = P#process.pid,
      case P#process.status of
	{timer,_} -> true;
	_ -> false
      end;
    _ -> false
  end.

collapse_locals(Actions,State,Conf) ->
  PartRestrict =
    mce_conf:well_behaved(Conf) andalso mce_conf:partial_order(Conf),
  {Type,Transitions} =
    int_transitions(State,Conf),
  case {Type,PartRestrict,Transitions} of
    {normal,true,Alternatives=[_|_]} ->
      {NewerActions,NewerState} =
	lists:foldl
	  (fun (Alternative,{AccActions,AccState}) ->
	       {NewActions,NewState} = commit(Alternative,AccState,Conf),
	       {AccActions++NewActions,NewState}
	   end, {Actions,State}, Transitions),
      collapse_locals(NewerActions,NewerState,Conf);
    _ ->
      {Actions,State}
  end.

moveIO(Flag, Received, State) ->
  receive
    {rcv, {Pid, Msg}} ->
      NewState = mce_erl_sysOS:addMsgToState(Pid, Msg, State),
      moveIO(changed, 
	     [{Pid,Msg}|Received], 
	     NewState);
    Msg when is_tuple(Msg), element(1, Msg) =:= gs ->
      case handleGS(State, Msg) of
	{changed, Recv, NewState} ->
	  moveIO(changed, [Recv|Received], NewState);
	_ ->
	  moveIO(Flag, Received, State)
      end;
    Msg ->
      io:format("mcerl: dont understand message ~p received~n", [Msg]),
      moveIO(Flag, Received, State)
  after 0 -> {Flag,lists:reverse(Received),State}
  end.

checkOverflow() ->
  case process_info(self(),message_queue_len) of
    {message_queue_len,N} ->
      if
	N>20, N rem 20 =:= 0 ->
	  io:format("Warning: external queue is of size ~p~n",[N]),
	  ok;
	true ->
	  ok
      end;
    Other ->
      io:format("Warning: cannot read external queue length~n"),
      ok
  end.

handleGS(State, Msg) ->
  Dict = State#state.dict,
  case mce_utils:find
    (fun (T) -> case T of {gs, _} -> true; _ -> false end end,Dict)	of
    {ok, {gs, {Pid, _}}} ->
      io:format
	("Have handler ~p for gs message; will handle~n  ~p~n",
	 [Pid,Msg]),
      {changed, {Pid,Msg}, mce_erl_sysOS:addMsgToState(Pid, Msg, State)};
    _ ->
      io:format("Did not find handler for gs message~n  ~p~n", [Msg]),
      false
  end.

%%% These are meant for simulation. Should not cause side effects
%%% (i.e. IO) except in the transition actually chosen, EXCEPT
%%% when the debugger is enabled.

allPossibilities(State, Conf) ->
  Nodes =
    try
      State#state.nodes
    catch
      Error:Pattern ->
	io:format("Have error ~p:~p at state~n  ~p~n", [Error, Pattern, State]),
	throw(bad)
    end,

  AllSynchMoves =
    allSynchMoves(State#state.nodes,[],State),

  AllCommMovesPossibilities =
    allCommMovesPossibles(orddict:to_list(State#state.ether), [], State),
  AllRunPossibilities =
    allNodeMoves(fun enumerateAllPossibles/3, Nodes, [], State),
  AllTerminatePossibilities =
    case mce_conf:terminate(Conf) of
      true -> allNodeMoves(fun allTerminatePossibles/3, Nodes, [], State);
      _ -> []
    end,
  AllPossibilities =
    AllSynchMoves ++
    AllCommMovesPossibilities ++
    AllTerminatePossibilities ++
    AllRunPossibilities,
  TimeRestrictedPossibilities =
    timeRestrict(State, AllPossibilities, Conf),
  ?LOG("all transitions=~n~p~n",[TimeRestrictedPossibilities]),
  partRestrict(TimeRestrictedPossibilities, Conf).

partRestrict(Possibilities, Conf) ->
  PartRestrict =
    mce_conf:well_behaved(Conf) andalso mce_conf:partial_order(Conf),
  if
    PartRestrict ->
      {Decisive,Normal} = part_sort_actions(Possibilities),
      case Normal of
	[_|_] -> {normal,Normal};
	_ -> {decisive,Decisive}
      end;
    true -> {decisive,Possibilities}
  end.

part_sort_actions(Possibilities) -> part_sort_actions(Possibilities,[],[]).
part_sort_actions([],Decisive,Normal) -> {Decisive,Normal};
part_sort_actions([Trans|Rest],Decisive,Normal) ->
  case Trans of
    {exec, Exec} ->
      Process = Exec#executable.process,
      case Process#process.status of
	runnable ->
	  case Process#process.expr of
	    {?CONTEXTTAG,{_,[{?WASCHOICETAG,_}|_]}} ->
	      part_sort_actions(Rest,[Trans|Decisive],Normal);
	    _ ->
	      part_sort_actions(Rest,Decisive,[Trans|Normal])
	  end;
	sendable ->
	  %% We can treat spawn specially
	  case find_innermost(Process#process.expr) of
	    {?SENDTAG,{{mcerlang,spawn,_},_}} ->
	      part_sort_actions(Rest,Decisive,[Trans|Normal]);
	    Innermost ->
	      part_sort_actions(Rest,[Trans|Decisive],Normal)
	  end;
	exiting ->
	  part_sort_actions(Rest,Decisive,[Trans|Normal]);
	dead ->
	  part_sort_actions(Rest,Decisive,[Trans|Normal]);
	receivable ->
	  part_sort_actions(Rest,Decisive,[Trans|Normal]);
	{timer,TimerDeadline} ->
	  part_sort_actions(Rest,[Trans|Decisive],Normal);
	Other ->
	  io:format("Strange process status ~p~n",[Process#process.status]),
	  throw(bad)
      end;
    Other -> 
      part_sort_actions(Rest,[Trans|Decisive],Normal)
  end.

find_innermost({?CONTEXTTAG,{Value,_}}) ->
  Value;
find_innermost(Value) ->
  Value.

timeRestrict(State, Possibilities, Conf) ->
  {RealTime,Now} =
    case {mce_conf:is_simulation(Conf),mce_conf:discrete_time(Conf)} of
      {_,true} ->
	{false,State#state.time};
      {false,_} ->
	{false,infinity};
      {true,_} ->
	{true,erlang:now()}
    end,
  {RestNonTimed,{RestMostUrgent,RestTimeEntries}} =
    lists:foldl
      (fun (Entry,Acc={NonTimed,TimeEnt={MostUrgent,TimerEntries}}) ->
	   case Entry of
	     {exec, Exec} ->
	       Process = 
		 Exec#executable.process,
	       MinWait =
		 case Process#process.status of
		   {timer, TimerDeadline} -> TimerDeadline;
		   _ -> Now
		 end,
	       MaxWait1 = 
		 case is_urgent(Now,Process#process.expr,Conf) of
		   false -> infinity;
		   {true,MaxWait2} -> MaxWait2
		 end,
	       MaxWait =
		 if
		   MaxWait1==infinity -> infinity;
		   true ->
		     case compareTimes_ge(MinWait,MaxWait1) of
		       true -> MinWait;
		       false -> MaxWait1
		     end
		 end,
	       TimedEntry = {MinWait,MaxWait,Entry},
	       ?LOG("Entry is~n~p~n",[TimedEntry]),
	       if 
		 MinWait == infinity -> 
		   Acc;
		 MaxWait == infinity andalso MinWait == Now ->
		   {[Entry|NonTimed],TimeEnt};
		 MostUrgent == infinity, MaxWait==infinity ->
		   {NonTimed,{infinity,[TimedEntry|TimerEntries]}};
		 MostUrgent == infinity ->
		   {NonTimed,
		    {MaxWait,
		     [TimedEntry|remove_timed_transitions(MaxWait,TimerEntries)]}};
		 true ->
		   case compareTimes_ge(MostUrgent, MinWait) of
		     false ->
		       Acc;
		     true when MaxWait==infinity ->
		       {NonTimed,{MostUrgent,[TimedEntry|TimerEntries]}};
		     true ->
		       case compareTimes_ge(MostUrgent,MaxWait) of
			 true when MostUrgent=/=MaxWait ->
			   {NonTimed,
			    {MaxWait,
			     [TimedEntry|remove_timed_transitions(MaxWait,TimerEntries)]}};
			 _ ->
			   {NonTimed,{MostUrgent,[TimedEntry|TimerEntries]}}
		       end
		   end
	       end;
	     _ -> {[Entry|NonTimed],TimeEnt}
	   end
       end, {[],{infinity,[]}}, Possibilities),
  if
    RealTime==true, RestNonTimed==[], RestTimeEntries=/=[] ->
      %% No process is ready to run, but there are timers enabled
      %% that will eventually fire, lets wait until the first one
      %% fires
      {MinWait,_,_} = getFirstProcessToFire(RestTimeEntries),
      WaitTime = timer:now_diff(MinWait, Now) div 1000,
      ?LOG("Will wait ~p milliseconds~n",[WaitTime]),
      timer:sleep(WaitTime);
    true -> ok
  end,
  RestNonTimed++(lists:map(fun ({_,_,Entry}) -> Entry end, RestTimeEntries)).

getFirstProcessToFire(Entries) ->
  %% We could introduce some randomness here...
  lists:foldl
    (fun (Entry={MinWait,_,_},Saved) ->
	 case Saved of
	   {SavedMinWait,_,_} ->
	     case compareTimes_ge(SavedMinWait,MinWait) of
	       true -> Entry;
	       false -> Saved
	     end
	 end
     end, void, Entries).

remove_timed_transitions(MaxWait,Entries) ->
  lists:filter(fun (Entry={MinWait,_,_}) -> compareTimes_ge(MaxWait,MinWait) end, Entries).

is_urgent(Now,Expr,Conf) ->
  IsInfinitelyFast = Conf#mce_opts.is_infinitely_fast,
  UrgentInfo={HasUrgentTag,MaxWait} =
    case Expr of
      {?CONTEXTTAG,{_,Context}} ->
	check_context_for_urgency_tag(Context);
      _ ->
	{false,void}
    end,
  ?LOG("is_urgent: Expr ~p has tag ~p~n",[Expr,UrgentInfo]),
  if 
    HasUrgentTag -> UrgentInfo;
    IsInfinitelyFast -> {true,Now};
    true -> false
  end.

check_context_for_urgency_tag(Context) ->
  case Context of
    [{?URGENTTAG,MaxWait}|_] ->
      {true,MaxWait};
    [{?WASCHOICETAG,_}|RestContext] -> 
      check_context_for_urgency_tag(RestContext);
    _ ->
      {false,void}
  end.

allNodeMoves(F,[],_,State) -> [];
allNodeMoves(F,[Node|RestNodes],Seen,State) ->
  NodeMoves =
    lists:map
      (fun ({Process,OtherProcesses,State}) ->
	   {exec,
	    #executable{node=Node#node{processes=OtherProcesses},
			otherNodes=Seen++RestNodes,
			process=Process}}
       end,
       F(Node#node.processes,[],State)),
  NodeMoves++allNodeMoves(F,RestNodes,[Node|Seen],State).

allSynchMoves([],_,State) -> [];  
allSynchMoves([Node|Nodes],Seen,State) ->
  lists:map
    (fun (Move) -> {Move,Node,Seen++Nodes} end,
     allSynchMovesInNode(Node#node.processes,[],Node,State)).

allSynchMovesInNode([],_,_,_) -> [];
allSynchMovesInNode([P|Rest],Others,Node,State) -> 
  case P#process.status of
    {synch_blocked,S} ->
      allSynchCombinations(P,Rest,Others,Node,State)++
	allSynchMovesInNode(Rest,[P|Others],Node,State);
    _ ->
      allSynchMovesInNode(Rest,[P|Others],Node,State)
  end.

allSynchCombinations(P,[],_,_,_) -> [];
allSynchCombinations(P,[First|Rest],Others,Node,State) ->
  {synch_blocked,S} = P#process.status,
  case First#process.status of
    {synch_blocked,S1} ->
      synch(ins(S),P,outs(S1),First,Rest++Others,Node,State)++
      synch(ins(S1),First,outs(S),P,Rest++Others,Node,State)++
	allSynchCombinations(P,Rest,[First|Others],Node,State);
    _ ->
      allSynchCombinations(P,Rest,Others,Node,State)
  end.

synch([],_,_,_,_,_,_) -> [];
synch(_,_,[],_,_,_,_) -> [];
synch([{Port1,Offers1}|RestP],P,[{Port2,Offers2}|RestQ],Q,Others,Node,State) ->
  if 
    Port1==Port2 ->
      merge_offers(Port1,Offers1,Offers2,P,Q,Others,Node,State)++
	synch(RestP,P,RestQ,Q,Others,Node,State);
    Port1<Port2 ->
      synch(RestP,P,[{Port2,Offers2}|RestQ],Q,Others,Node,State);
    true ->
      synch([{Port1,Offers1}|RestP],P,RestQ,Q,Others,Node,State)
  end.

ins({Ins,_,_}) -> Ins.
outs({_,Outs,_}) -> Outs.

merge_offers(_,[],OutOffers,P,Q,_,Node,State) -> [];
merge_offers(Port,[FirstOffer|RestOffers],OutOffers,P,Q,Others,Node,State) ->
  merge_offers1(Port,FirstOffer,OutOffers,P,Q,Others,Node,State)++
    merge_offers(Port,RestOffers,OutOffers,P,Q,Others,Node,State).

merge_offers1(_,InOffers,[],P,Q,_,Node,State) -> [];
merge_offers1(Port,InOffer,[FirstOutOffer|RestOutOffers],P,Q,Others,Node,State) ->
  {GuardFun,ExecInFun} = InOffer,
  {Value,ExecOutFun} = FirstOutOffer,
  try GuardFun(Value) of
      true ->
      [{synch,
	Port,
	P,{ExecInFun,Value},
	Q,ExecOutFun,
	Others}|
       merge_offers1(Port,InOffer,RestOutOffers,P,Q,Others,Node,State)];
      false ->
      merge_offers1(Port,InOffer,RestOutOffers,P,Q,Others,Node,State);
      Other ->
      throw(bad)
  catch Cause:Reason ->
      io:format
	("*** Error: evaluating guard function~n~p~nthrows an exception ~p:~p~n",
	 [GuardFun,Cause,Reason]),
      Trace = erlang:get_stacktrace(),
      io:format
	("Stack trace:\n"++(mce_erl_debugger:printStackTrace(2,Trace))++"\n"),
      throw(bad)
  end.
  
allCommMovesPossibles([],Seen,_) -> [];
allCommMovesPossibles([NodeSpec|Rest],Seen,S) ->
  case NodeSpec of
    {Key={FromObj,ToObj},[Sig|RestSigs]} ->
      RemainingEther =
	if RestSigs=:=[] -> Seen++Rest;
	   true -> Seen++[{Key,RestSigs}|Rest]
	end,
      [{commMove,{FromObj,ToObj,Sig,RemainingEther}}|
       allCommMovesPossibles(Rest,Seen++[NodeSpec],S)];
    _ ->
      throw(badEther)
  end;
allCommMovesPossibles(Ether,Seen,State) ->
  io:format("Ether is ~p~n",[Ether]),
  throw(badEther).

%%allTerminatePossibles([],_,State) -> [];
%%allTerminatePossibles([P|Rest],Seen,State) ->
%%  [{P#process{status=dead},Seen++Rest,State}|
%%   allTerminatePossibles(Rest,[P|Seen],State)].

allTerminatePossibles([],_,State) -> [];
allTerminatePossibles([P|Rest],Seen,State) ->
  case (P#process.flags)#processFlags.do_terminate of
    true ->
      [{P#process{status=dead},Seen++Rest,State}|
       allTerminatePossibles(Rest,[P|Seen],State)];
    false ->
      allTerminatePossibles(Rest,[P|Seen],State)
  end.

enumerateAllPossibles([], _, State) -> [];
enumerateAllPossibles([P| Rest], Seen, State) ->
  case P#process.status of
    blocked ->
      enumerateAllPossibles(Rest, [P| Seen], State);
    {synch_blocked,_} ->
      enumerateAllPossibles(Rest, [P| Seen], State);
    choice ->
      Others = Seen ++ Rest,
      Values =
	lists:map
	  (fun (LetExp) ->
	       CExp =
		 case LetExp of
		   {?CONTEXTTAG,{Exp,Cntxt}} ->
		     {?CONTEXTTAG,
		      {Exp,
		       [mce_erl:was_choice(void)|Cntxt]}};
		   _ ->
		     mce_erl:mk_context
		       (LetExp,[mce_erl:was_choice(void)])
		 end,
	       NewP =
		 P#process{status=runnable, expr=CExp},
	       {NewP, Others, State}
	   end,
	   digOutChoice(P#process.expr)) ++
	enumerateAllPossibles(Rest, [P| Seen], State);
    _ ->
      [{P, Seen ++ Rest, State}|
       enumerateAllPossibles(Rest, [P| Seen], State)]
  end.

digOutChoice(E={?CHOICETAG, _}) ->
  case mce_erl:choice_alternatives(E) of
    N when is_integer(N), N>0 ->
      lists:map(fun (N) -> fun () -> N end end, lists:seq(1,N));
    L when is_list(L) ->
      mce_erl:choice_alternatives(E)
  end;
digOutChoice({?CONTEXTTAG, {E={?CHOICETAG, _}, Context}}) ->
  case mce_erl:choice_alternatives(E) of
    N when is_integer(N) ->
      lists:map
	(fun (Alternative) ->
	     mce_erl:mk_context(fun () -> Alternative end, Context)
	 end,
	 lists:seq(1,N));
    L when is_list(L) -> 
      lists:map
	(fun (Alternative) -> mce_erl:mk_context(Alternative, Context) end,
	 mce_erl:choice_alternatives(E))
  end.

compareTimes_ge({M1, S1, Mic1}, {M2, S2, Mic2}) ->
  M1 > M2
    orelse M1 =:= M2 andalso S1 > S2
    orelse M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2.

compareTimes_gt(Now1,Now2) ->
  (Now1 =/= Now2) andalso compareTimes_ge(Now1,Now2).

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

minusTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  {Mic,NewS1,NewM1} =
    if
      Mic1>=Mic2 -> {Mic1-Mic2,S1,M1};
      S1>0 -> {(Mic1-Mic2)+1000000,S1-1,M1};
      M1>0 -> {(Mic1-Mic2)+1000000,S1+1000000-1,M1-1};
      true -> {Mic1-Mic2,0,0}
    end,
  {Sec,NewNewM1} =
    if
      NewS1>=S2 -> {NewS1-S2,NewM1};
      NewM1>0 -> {(NewS1-S2)+1000000,NewM1-1};
      true -> {NewS1-S2,0}
    end,
  {NewNewM1-M2,Sec,Mic}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doStep({commMove, CommSpec}, State, Conf) ->
  initActions(),
  Result = mce_erl_node:doDispatchSignal(CommSpec, State),
  {getActions(), Result};
doStep({exec, Exec}, SavedState, Conf) ->
  initActions(),
  try
    Result = doStep1(Exec, SavedState, Conf),
    ?LOG("doStep: result=~p~n", [Result]),
    {getActions(), Result}
  catch
    {user, Error, Reason} ->
      Process = mce_erl_state:getProcess(mce_erl_state:getState()),
      ?LOG("Process~n  ~p~nmaybe stopped because of exception ~p:~p~n",
	   [Process, Error, Reason]),
      Pid = (Exec#executable.process)#process.pid,
      handle_exception(Process, Pid, Error, Reason, Conf)
  end;
doStep({{synch,Port,P,{ExecInFun,Value},Q,ExecOutFun,Others},Node,OtherNodes},State,Conf) ->
  initActions(),
  ContextP =
    case P#process.expr of
      {?CONTEXTTAG,{_,CntxtP}} -> CntxtP;
      _ -> []
    end,
  ContextQ =
    case Q#process.expr of
      {?CONTEXTTAG,{_,CntxtQ}} -> CntxtQ;
      _ -> []
    end,
  mce_erl_actions:record
    (mce_erl_actions:mk_synch(Port,Value,P#process.pid,Q#process.pid)),
  NewP =
    P#process
    {expr=mce_erl:mk_context({ExecInFun,[Value]},ContextP),
     status=runnable},
  NewQ =
    Q#process
    {expr=mce_erl:mk_context({ExecOutFun,[]},ContextQ),
     status=runnable},
  {getActions(), mce_erl_sysOS:mkStateFromProcesses(NewP,NewQ,Others,Node,OtherNodes,State)}.

maybe_notice_exit(Exception, Reason, Conf) ->
  NormalExit =
    (Exception=:=exit) and (Reason=:=normal),
  FailOnExit =
    mce_conf:fail_on_exit(Conf),
  NoticeExit =
    mce_conf:notice_exits(Conf),
  WillExit =
    FailOnExit and (not(NormalExit)),
  PrintExit =
    NoticeExit and (not(NormalExit or WillExit)),
  Trace =
    if
      PrintExit ->
	erlang:get_stacktrace();
      WillExit ->
	Tr=erlang:get_stacktrace(),
	put(stack_trace,Tr),
	Tr;
      true ->
	[]
    end,
  TraceStr =
    if
      PrintExit ->
	"Stack trace:\n"++(mce_erl_debugger:printStackTrace(2,Trace))++"\n";
      true ->
	""
    end,
  if
    not(PrintExit) -> ok;
    Exception=:=exit ->
      io:format
	("Process~n  ~p~n exited because of: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr]);
    Exception=:=error ->
      io:format
	("~nProcess~n  ~p~nexited because of error: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr]);
    Exception=:=throw ->
      io:format
	("~nProcess~n  ~p~nthrew an uncaught exception: ~p~n~s~n",
	 [mce_erl_state:getProcess(mce_erl_state:getState()),Reason,TraceStr])
  end.

handle_exception(Process,Pid,Exception,Reason,Conf) ->
  case mce_conf:fail_on_exit(Conf) of
    true ->
      case Exception of
	exit ->
	  if Reason =/= normal ->
	      mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
	      signal_user_error(Exception,Reason);
	     true ->
	      {getActions(), exceptionReturn(Process,Reason,Conf)}
	  end;
	_ ->
	  mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
	  signal_user_error(Exception,Reason)
      end;
    false ->
      if
	Exception =:= throw ->
	  {getActions(), exceptionReturn(Process,{nocatch, Reason},Conf)};
	true ->
	  {getActions(), exceptionReturn(Process,Reason,Conf)}
      end
  end.

signal_user_error(Exception,Reason) ->
  StackTrace = 
    case get(stack_trace) of
      Tr when is_list(Tr) -> Tr;
      _ -> []
    end,
  mce_result:throw_result_exc
    (mce_result:mk_user_error
     (mce_result:mk_exception_error
      (Exception,Reason,StackTrace))).

exceptionReturn(Process,Reason,Conf) ->
  State = mce_erl_state:getState(),
  case mce_erl_state:getNode(State) of
    none ->
      mce_erl_sysOS:mkStateFromOtherNodes(State);
    _ ->
      case mce_conf:sends_are_sefs() of
	false ->
	  mce_erl_sysOS:mkStateFromCurrentExecutable
	    (mce_erl_sys:inform(Reason, State));
	true ->
	  mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	    (putProcess(Process,mce_erl:exiting(Reason),State,Conf),
	     State)
      end
  end.

doStep1(Exec, SavedState, Conf) ->
  P = Exec#executable.process,
  ?LOG("Going to step ~p with status ~p for a while...~n",
       [P, P#process.status]),
  ?LOG("Executable is ~p~n", [Exec]),
  case P#process.status of
    runnable ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec,SavedState)),
      case P#process.expr of
	{?CONTEXTTAG,{Expr,[{?WASCHOICETAG,_}|Context]}} ->
	  mce_erl_actions:record
	    (mce_erl_actions:mk_choice
	       (P#process.pid,P#process.expr));
	_ ->
	  mce_erl_actions:record
	    (mce_erl_actions:mk_run
	       (P#process.pid,P#process.expr))
      end,
      runUserCode(P#process.expr, Conf);
    sendable ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      doSend(P#process.pid, P#process.expr, SavedState, Conf);
    exiting ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      doExit(P#process.pid, P#process.expr, SavedState, Conf);
    {timer, Deadline} ->
      doRunTimer(P, Deadline, Exec, SavedState, Conf);
    receivable ->
      doReceive(Exec, SavedState, Conf);
    dead ->
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(Exec, SavedState)),
      mce_erl_actions:record
	(mce_erl_actions:mk_terminated
	 (P#process.pid,[])),
      handleTerminated()
  end.

doRunTimer(P, Deadline, Exec, SavedState, Conf) ->
  Pid = P#process.pid,
  Time = SavedState#state.time,
  NewState =
    case mce_conf:discrete_time(Conf) of
      true ->
	case compareTimes_ge(Deadline,Time) of
	  true -> 
	    mce_erl_actions:record
	      (mce_erl_actions:mk_timeout
		 (Pid,
		  minusTimeStamps(Deadline,Time))),
	    SavedState#state{time=Deadline};
	  false ->
	    mce_erl_actions:record
	      (mce_erl_actions:mk_timeout
		 (Pid,{0,0,0})),
	    SavedState
	end;
      _ -> SavedState
    end,
  %% Timer has expired we can just run the code
  {TimeoutCall, Context} = getTimeoutCall(P#process.expr),
  mce_erl_state:setState(mce_erl_sysOS:setCurrentRunContext(Exec,NewState)),
  mce_erl_actions:record
    (mce_erl_actions:mk_run
     (Pid,TimeoutCall)),
  try mce_erl_stacks:execStack(TimeoutCall,Context) of
      Value ->
      State = mce_erl_state:getState(),
      case mce_erl_stacks:isTagged(Value) of
	true ->
	  {NewValue, NewContext} = mce_erl_stacks:parseStack(Value, State#system.time),
	  NewExpr = mce_erl:mk_context(NewValue, NewContext),
	  PNew = putProcess(P, NewExpr, State, Conf),
	  mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	    (putProcess(PNew,NewExpr,State,Conf), State);
	false -> 
	  exiting(P,Value,State,Conf)
      end
  catch
    Error:Reason ->
      maybe_notice_exit(Error, Reason, Conf),
      throw({user, Error, Reason})
  end.

getTimeoutCall({?RECVTAG,{_,{_Timer,TimeoutCall}}}) ->
  {TimeoutCall,[]};
getTimeoutCall({?CONTEXTTAG,{Innermost,Context}}) ->
  {?RECVTAG,{_,{_Timer,TimeoutCall}}} = Innermost,
  {TimeoutCall,Context}.

doExit(Pid, Expr, SavedState, Conf) ->
  Reason = mce_erl:exiting_reason(Expr),
  State = mce_erl_state:getState(),
  mce_erl_actions:record(mce_erl_actions:mk_died(Pid,Reason)),
  mce_erl_sysOS:mkStateFromCurrentExecutable
    (mce_erl_sys:inform(Reason, State)).

doSend(Pid, Expr, SavedState, Conf) ->
  ?LOG("doSend(~p)~n",[Expr]),
  {Innermost, Context} =
    case Expr of
      {?CONTEXTTAG,{E,Cntxt}} ->
	{mce_erl:send_sef_fun(E), Cntxt};
      E ->
	{mce_erl:send_sef_fun(E), []}
    end,
  mce_erl_actions:record(mce_erl_actions:mk_run(Pid,Innermost)),
  runUserCode1(Innermost,Context,Conf).

runUserCode(Expr, Conf) ->
  {Innermost, Context} =
    case Expr of
      {?CONTEXTTAG, Cnt} -> Cnt;
      _ -> {Expr, []}
    end,
  runUserCode1(Innermost,Context,Conf).

runUserCode1(Innermost,Context,Conf) ->
  ?LOG("will execute stack ~p~nwith context ~p~n",
       [Innermost, Context]),
  try
    mce_erl_stacks:execStack(Innermost, Context)
    of
    Value ->
      State = mce_erl_state:getState(),
      ?LOG(("Call ~p~nwith context ~p got back value~n~p," ++
	    "actions are~n~p~nstate ~p~n"),
	   [Innermost, Context, Value, get(actions), State]),
      P = mce_erl_state:getProcess(State),
      case mce_erl_stacks:isTagged(Value) of
	false ->
	  exiting(P,Value,State,Conf);
	true ->
	  {NewValue, NewContext} = mce_erl_stacks:parseStack(Value, State#system.time),
	  NewExec = mce_erl:mk_context(NewValue, NewContext),
	  ?LOG("NewValue: ~p ~nNewContext:~n ~p~nNewExec=~p~n",
	       [NewValue, NewContext, NewExec]),
	  mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	    (putProcess(P, NewExec, State, Conf), State)
      end
  catch
    Error:Reason ->
      maybe_notice_exit(Error, Reason, Conf),
      throw({user, Error, Reason})
  end.

exiting(P,Value,State,Conf) ->
  case mce_conf:sends_are_sefs() of
    false ->
      ?LOG("Process ~p terminates with value~n~p~n",
	   [P#process.pid, Value]),
      mce_erl_actions:record
	(mce_erl_actions:mk_terminated
	   (P#process.pid,Value)),
      NewS = mce_erl_sys:inform(normal, State),
      %% Note that we have to recompute links and pid map
      %% here because it can potentially change in
      %% mce__erl_sys:inform.
      mce_erl_sysOS:mkStateFromCurrentExecutable(NewS);
    true ->
      mce_erl_sysOS:mkStateFromCurrentExecutableWithProcess
	(putProcess(P,mce_erl:exiting(normal),State,Conf),
	 State)
  end.

handleTerminated() ->
  State = mce_erl_state:getState(),
  mce_erl_sysOS:mkStateFromCurrentExecutable
    (mce_erl_sys:inform({'EXIT', crashed}, State)).

isTagged({MaybeTag,_}) ->
  case MaybeTag of
    ?TRYTAG -> true;
    ?LETTAG -> true;
    ?CHOICETAG -> true;
    ?WASCHOICETAG -> true;
    ?SENDTAG -> true;
    ?EXITINGTAG -> true;
    ?RECVTAG -> true;
    ?URGENTTAG -> true;
    ?SYNCHTAG -> true;
    _ -> false
  end;
isTagged(_) -> false.


doReceive(Exec, SavedState, Conf) ->
  P = Exec#executable.process,
  if P#process.status =:= receivable ->
      {true, {ContFun, Hd, Args, NewQueue}, Context} =
	checkReceive(P),
      NewP =
	P#process{expr={?RECVTAG, {{Hd, Args}, Context}},
		  status=runnable, queue=NewQueue},
      NewExec =
	Exec#executable{process=NewP},
      mce_erl_state:setState
	(mce_erl_sysOS:setCurrentRunContext(NewExec, SavedState)),
      mce_erl_actions:record
	(mce_erl_actions:mk_recv
	 (P#process.pid,Hd)),
      runUserCode(mce_erl:mk_context({ContFun, []}, Context), Conf)
  end.

putProcess(P, Exec, State, Conf) ->
  {Innermost, Context} =
    case Exec of
      {?CONTEXTTAG, Value} -> Value;
      _ -> {Exec, []}
    end,
  case Innermost of
    {?RECVTAG, {Fun, Timer}} ->
      case checkReceiveContext(Innermost, Context, P) of
	false ->
	  case Timer of
	    {infinity, _} ->
	      P#process{status=blocked, expr=Exec};
	    {Time, _} ->
	      {Deadline,ExecNew} =
		case {mce_conf:is_simulation(Conf),mce_conf:discrete_time(Conf)} of
		  {_,true} ->
		    NewDeadline = 
		      addTimeStamps
			(milliSecondsToTimeStamp(Time),State#system.time),
		    NewDeadline;
		  {true,_} ->
		    {addTimeStamps(milliSecondsToTimeStamp(Time),erlang:now()),
		     Exec};
		  {false,_} -> 
		    {if Time=/=0 -> infinity; true -> 0 end, Exec}
		end,
	      P#process{status={timer,Deadline}, expr=ExecNew}
	  end;
	_ -> P#process{status=receivable, expr=Exec}
      end;
    {?CHOICETAG, _} ->
      P#process{status=choice, expr=Exec};
    {?SENDTAG, _} ->
      P#process{status=sendable, expr=Exec};
    {?SYNCHTAG, S} ->
      P#process{status={synch_blocked,S}, expr=Exec};
    {?EXITINGTAG, _} ->
      P#process{status=exiting, expr=Exec};
    _ ->
      io:format("Internal fault: putProcess(_,~p,_)~n", [Exec]),
      exit(putProcess)
  end.

%%% A new data value have been appended to the queue of P;
%%% we have to check whether the new value is readable and do
%%% a status update.
updProcStatusFromQueue(P) ->
  case updateableStatus(P#process.status) of
    true ->
      case checkReceive(P) of
        false -> P;
        _ -> P#process{status=receivable}
      end;
    false -> 
      P
  end.

updateableStatus(Status) ->
  case Status of
    {inputable,_,_} ->
      true;
    blocked ->
      true;
    {timer,_} ->
      true;
    _ ->
      false
  end.

checkReceive(P) ->
  checkReceiveContext(P#process.expr,P).

checkReceiveContext({?CONTEXTTAG,{Innermost,Context}},P) ->
  checkReceiveContext(Innermost,Context,P);
checkReceiveContext(Expr,P) ->
  checkReceiveContext(Expr,[],P).

checkReceiveContext({?RECVTAG,{Fun,_}},Context,P) ->
  case checkReceiveFun(Fun,P#process.queue) of
    {true,Result} -> {true,Result,Context};
    Other -> Other
  end.

checkReceiveFun(Fun,Queue) ->
  checkReceiveFun(Fun,Queue,[]).
checkReceiveFun(Fun,[],Saved) -> false;
checkReceiveFun(Fun={Mod,RcvFun,Args},[Hd|Rest],Saved) ->
  case apply(Mod,RcvFun,[Hd|Args]) of
    false -> checkReceiveFun(Fun,Rest,[Hd|Saved]);
    {true,ContFun} -> {true,{ContFun,Hd,Args,lists:reverse(Saved,Rest)}}
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initActions() ->
  put(actions,[]).

record_action(Action) ->
  ?LOG("record_action(~p)~n", [Action]),
  put(actions,[Action|get(actions)]).

getActions() ->
  lists:reverse(get(actions)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_partition(List) ->
  random_partition(List, [], []).

random_partition([], Left, Right) -> {Left, Right};
random_partition([X| Xs], Left, Right) ->
  U = mce_random:uniform(2),
  if U =:= 1 ->
      random_partition(Xs, Left, [X| Right]);
     true ->
      random_partition(Xs, [X| Left], Right)
  end.

randomise([]) ->
  [];
randomise([X]) ->
  [X];
randomise(Bigger) ->
  {Left, Right} = random_partition(Bigger),
  randomise(Left) ++ randomise(Right).

