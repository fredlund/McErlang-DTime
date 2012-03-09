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

%%% Does not implement restarts yet

-module(mce_erl_supervisor).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), true).
-endif.

-export([start_link/2,start_link/3,doStart/3,doStart/4]).
-export([start_child/2,int_start_child/1]).

start_link(Module, Arg) ->
  ?LOG("start_link(~p,~p)~n",[Module,Arg]),
  Pid = spawn_link(?MODULE, doStart, [Module, Arg, self()]),
  waitForStart(Pid).

start_link({local,Name}, Module, Arg) ->
  ?LOG("start_link(~p,~p,~p)~n",[{local,Name},Module,Arg]),
  Pid = spawn_link(?MODULE, doStart, [Name, Module, Arg, self()]),
  waitForStart(Pid).

waitForStart(Pid) ->
  receive
    {ok,started} -> {ok, Pid}
  end.

doStart(Name, Module, Arg, SupervisorPid) ->
  process_flag(trap_exit,true),
  ?LOG("~p: going to register name ~p for ~p~n",[?MODULE,Name,self()]),  
  register(Name, self()),
  Result = apply(Module, init, [Arg]),
  ?LOG("start recipe: ~p~n",[Result]),
  startChildren(Result, SupervisorPid, Module).

doStart(Module, Arg, SupervisorPid) ->
  process_flag(trap_exit,true),
  Result = apply(Module, init, [Arg]),
  ?LOG("start recipe: ~p~n",[Result]),
  startChildren(Result, SupervisorPid, Module).

loop(ModuleName,Spec) ->
  receive
    {start_child,{Pid,ChildSpec}} ->
      ?LOG("got a request to start ~p~n",[ChildSpec]),
      {{RestartStrategy,_,_},Children} = Spec,
      if
	RestartStrategy==simple_one_for_one ->
	  Child = hd(Children),
	  {_,{M,F,A},_,_,_,_} = Child,
	  MFANew = {M,F,A++ChildSpec},
	  Result = int_start_child(setelement(2,Child,MFANew)),
	  Pid!{child_started,Result},
	  loop(ModuleName,Spec)
      end
  end.

startChildren({ok,Spec={{Strategy,_,_},Children}}, SuperPid, ModuleName) ->
  if
    Strategy==simple_one_for_one ->
      SuperPid!{ok,started},
      loop(ModuleName,Spec);
    true ->
      startChildren1(Children, SuperPid, ModuleName, Spec)
  end.

startChildren1([],SupervisorPid,ModuleName,Spec) ->
  SupervisorPid!{ok,started}, 
  loop(ModuleName,Spec);
startChildren1([terminate|_],_,ModuleName,Spec) ->
  loop(ModuleName,Spec);
startChildren1([ChildSpec|Rest],SupervisorPid,ModuleName,Spec) ->
  int_start_child(ChildSpec),
  startChildren1(Rest,SupervisorPid,ModuleName,Spec).

int_start_child(ChildSpec) ->
  ?LOG("int_start_child(~p)~n",[ChildSpec]),
  case ChildSpec of
    {_Id,{Module,Fun,Args},_PT,_ShutDown,_Type,_Modules} ->
      Result = {ok,_Child} = apply(Module,Fun,Args),

      Result
  end.

start_child(SupRef,ChildSpec) ->
  SupRef!{start_child,{self(),ChildSpec}},
  receive
    {child_started,Result} -> Result
  end.


