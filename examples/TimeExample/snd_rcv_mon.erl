-module(snd_rcv_mon).

-language(erlang).

-export([init/1,monitorType/0,stateChange/3]).

-behaviour(mce_behav_monitor).

-include("monState.hrl").
-include("stackEntry.hrl").
-include("state.hrl").

monitorType() -> safety.

init(_) -> {ok,not_seen}.

stateChange(_, not_seen, Stack) ->
  case has_receive(Stack) of
    true -> {ok, {0,0,0}};
    false -> {ok,not_seen}
  end;
stateChange(State, N={_,_,MicroSeconds}, Stack) when MicroSeconds>=5000 ->
  too_long_wait;
stateChange(State, N, Stack) ->
  case has_receive(Stack) of
    true -> {ok, not_seen};
    false -> 
      case has_time_upd(State,Stack) of
	{yes,Add} ->
	  NewSumStamp = addTimeStamps(N,Add),
	  {ok, NewSumStamp};
	_ ->
	  {ok,N}
      end
  end.
  
has_time_upd(State,Stack) ->
  {Element2, Rest} = mce_behav_stackOps:pop(Stack),
  case mce_behav_stackOps:is_empty(Rest) of
    true -> no;
    false -> 
      {Element1,_} = mce_behav_stackOps:pop(Rest),
      MonState1 = Element1#stackEntry.state,
      State1 = MonState1#monState.state,
      Time1 = State1#state.time,
      MonState2 = Element2#stackEntry.state,
      State2 = MonState2#monState.state,
      Time2 = State2#state.time,
      TimeStampNew = minusTimeStamps(Time2,Time1),
      if
	Time1=/=Time2 ->
	  {yes,TimeStampNew};
	true ->
	  no
      end
  end.

has_receive(Stack) ->
  {Element, _} = mce_behav_stackOps:pop(Stack),
  Actions = Element#stackEntry.actions,
  lists:any
    (fun (Action) -> mce_erl_actions:is_recv(Action) end,
     Actions).

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
  
addTimeStamps({M1,S1,Mic1},{M2,S2,Mic2}) ->
  Mic=Mic1+Mic2,
  MicRem = Mic rem 1000000,
  MicDiv = Mic div 1000000,
  S = S1+S2+MicDiv,
  SRem = S rem 1000000,
  SDiv = S div 1000000,
  M = M1+M2+SDiv,
  {M,SRem,MicRem}.

  
  
