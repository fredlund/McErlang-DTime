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
%% @end
% % @doc
%% @private 
%% @type monState(). A record representing the state of the system (dictionary,nodes,links) and the current monitor used. Specified in file monState.hrl.
%% @type action(). A record representing an action performable by certain process (process id, name, arguments...). Specified in file action.hrl.
% % @type abstraction(). A record representing an abstraction technique (module, contents). Declared in file abstraction.hrl.

-module(mce_abs_norm).
-export([init/1, abstract_actions/2,abstract_state/2]).
-behaviour(mce_behav_abstraction).

-include("monState.hrl").
-include("../../languages/erlang/src/include/state.hrl").
-include("../../languages/erlang/src/include/node.hrl").
-include("../../languages/erlang/src/include/process.hrl").

%%-define(debug,true).
-include("macros.hrl").

init(_) ->
    {ok,ok}.

%% Returns a tuple containing the parameters. Thus, no abstraction is performed.
% %-spec(abstract_actions/2::([action()],abstraction())-> ({[action()],abstraction()})).	     
%% @doc Does not perform any action and returns a tuple containing the parameters unchanged.
%% @end
%% @spec ([action()], any())->{[action()], any()}
abstract_actions(A,AS) ->
    {A,AS}.

%% Returns a monState record whose state component is normalized.
% %-spec(abstract_state/2::(monState(),abstraction())-> ({monState(),abstraction()})).	     
%% @doc Performs an abstraction that consists in ordering alphabetically the 
%% processes within each of the nodes of the state.
%% @end
%% @spec (monState(), any())->{monState(), any()}
abstract_state(State,AS) -> 
  case catch {normalizeState(State),AS} of
    {'EXIT',Reason} ->
      io:format
	("abstract_state returns error ~p~nfor state ~p~n",
	 [Reason,State]),
      exit(abstract_state);
    Other ->
      Other
  end.

normalizeState(State) ->
  StateComp = State#monState.state,
  Now = StateComp#state.time,
  NewNow = 
    if Now=/=void -> {0,0,0};
       true -> Now
    end,
  NewClocks =
    lists:map
      (fun ({ClockId,Time}) -> {ClockId,minusTimeStamps(Now,Time)} end,
       StateComp#state.clocks),
  ?LOG("Clocks was ~p and is ~p~n",[StateComp#state.clocks,NewClocks]),
  State#monState
    {state=
     StateComp#state
     {time=NewNow,
      clocks=NewClocks,
      nodes=
      lists:sort
      (lists:map
       (fun (Node) ->
	    Node#node
	      {processes=
	       lists:sort
	       (lists:map 
		(fun (P) -> adjust_timers(P,Now)
		 end, Node#node.processes))}
	end,
	StateComp#state.nodes))}}.

adjust_timers(Process,void) ->
  Process;
adjust_timers(Process,Now) ->
  case Process#process.status of
    {timer,Deadline} ->
      NewDeadline = minusTimeStamps(Deadline,Now),
      Process#process{status={timer,NewDeadline}};
    _ ->
      Process
  end.

compareTimes_ge({M1, S1, Mic1}, {M2, S2, Mic2}) ->
  M1 > M2
    orelse M1 =:= M2 andalso S1 > S2
    orelse M1 =:= M2 andalso S1 =:= S2 andalso Mic1 >= Mic2.

minusTimeStamps(T1={M1,S1,Mic1},T2={M2,S2,Mic2}) ->
  case compareTimes_ge(T2,T1) of
    true -> {0,0,0};
    false ->
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
      {NewNewM1-M2,Sec,Mic}
  end.


