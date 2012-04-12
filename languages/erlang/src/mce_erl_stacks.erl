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

-module(mce_erl_stacks).

-export([mkSend/2,mkLet/2,mkTry/3,tryValue/3,tryHandler/2,parseStack/2]).
-export([mkSynch/1]).
-export([execStack/2,isTagged/1]).
-export([mkUrgent/1,mkUrgent/2,mkSlow/1,mkSlow/2]).
-include("emacros.hrl").

mkSend(Label, Fun={M,F,A}) ->
  try mce_conf:sends_are_sefs() of
    true -> mce_erl:send_sef(Label,Fun);
    false -> apply(M,F,A)
  catch _:_ -> 
      %% We don't have an McErlang context, so we just happily ignore
      %% the send side effect!
      apply(M,F,A)
  end.

mkLet(Value, Cont) ->
    case isTagged(Value) of
      false -> apply_value(Cont, Value);
      true -> mce_erl:letexpr(Value, Cont)
    end.

mkTry(F, BodyCont, HandlerCont) ->
    try
      F()
    of
      Value -> mce_erl_stacks:tryValue(Value, BodyCont, HandlerCont)
    catch
      Error:Reason -> mce_erl_stacks:tryHandler({Error, Reason}, HandlerCont)
    end.

mkUrgent(Cont) ->
  mce_erl:urgent(Cont,0).

mkUrgent(Cont,MaxWait) when is_integer(MaxWait) ->
  mce_erl:urgent(Cont,MaxWait).

mkSlow(Cont) ->
  mce_erl:urgent(Cont,infinite).

mkSlow(Cont,MaxWait) when is_integer(MaxWait) ->
  mce_erl:urgent(Cont,MaxWait).

tryValue(Value, BodyCont, HandlerCont) ->
    case isTagged(Value) of
      false -> apply_value(BodyCont, Value);
      true -> mce_erl:tryexpr(Value, BodyCont, HandlerCont)
    end.

tryHandler(Value,HandlerCont) ->
  apply_value(HandlerCont,Value).

apply_value(Cont,RawValue) ->
  Value = get_value(RawValue),
  case Cont of
    {Module,Fun,Args} ->
      apply(Module,Fun,[Value|Args]);
    {Fun,Args} ->
      apply(Fun,[Value|Args])
  end.

get_value(Value) ->
  case Value of
    {?URGENTTAG,{_,Expr}} ->
      get_value(Expr);
    _ ->
      Value
  end.

parseStack(Context,Time) ->
  parseStack(Context,[],Time).
parseStack(Entry={?RECVTAG,_},RestStack,Time) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Entry={?CHOICETAG,_},RestStack,Time) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Entry={?SENDTAG,_},RestStack,Time) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Entry={?SYNCHTAG,_},RestStack,Time) ->
  {Entry,lists:reverse(RestStack)};
parseStack({?LETTAG,{Expr,Cont}},RestStack,Time) ->
  parseStack(Expr,[{?LETTAG,{void,Cont}}|RestStack],Time);
parseStack(Entry={?WASCHOICETAG,Expr},RestStack,Time) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Elem={?URGENTTAG,{MaxWait,Expr}},RestStack,Time) ->
  case RestStack of
    [{?URGENTTAG,_}|Rest] ->
      parseStack(Elem,Rest,Time);
    _ ->
      MaxWaitTimer = addTimeStamps(milliSecondsToTimeStamp(MaxWait),Time),
      parseStack(Expr,[{?URGENTTAG,MaxWaitTimer}|RestStack],Time)
  end;
parseStack({?TRYTAG,{Expr,Cont}},RestStack,Time) ->
  parseStack(Expr,[{?TRYTAG,{void,Cont}}|RestStack],Time);
parseStack(Arg1,Arg2,_) ->
  io:format
    ("*** Error: malformed arguments to parseStack:~n  ~p; ~p~n",
     [Arg1,Arg2]),
  throw(bad).

execStack(Command,[]) ->
  case Command of
    {Module,Fun,Args} when is_atom(Module), is_atom(Fun), is_list(Args) ->
      apply(Module,Fun,Args);
    {Fun,Args} when is_function(Fun) ->
      apply(Fun,Args);
    Fun when is_function(Fun) ->
      apply(Fun,[]);
    _ ->
      io:format
	("*** Error: malformed command~n~p~nin execStack with empty context~n",
	 [Command]),
      throw(bad)
  end;
execStack(Command,[{?LETTAG,{_,Cont}}|Rest]) ->
  mkLet(execStack(Command,Rest),Cont);
execStack(Command,[{?TRYTAG,{_,{BodyCont,HandlerCont}}}|Rest]) ->
  try execStack(Command,Rest) of Value -> 
      tryValue(Value,BodyCont,HandlerCont)
  catch Error:Reason -> tryHandler({Error,Reason,void},HandlerCont)
  end;
execStack(Command,[{?WASCHOICETAG,_}|Rest]) ->
  execStack(Command,Rest);
execStack(Command,[{?URGENTTAG,_}|Rest]) ->
  execStack(Command,Rest);
execStack(Command,[{?SYNCHTAG,_}|Rest]) ->
  execStack(Command,Rest);
execStack(Command,OtherTag) ->
  io:format
    ("*** Error: malformed tag in execStack:~n~p~nfor command~n~p~n",
     [OtherTag,Command]),
  throw(bad).
  
isTagged({MaybeTag,Arg}) ->
  case MaybeTag of
    ?TRYTAG -> true;
    ?LETTAG -> true;
    ?CHOICETAG -> true;
    ?WASCHOICETAG -> true;
    ?SENDTAG -> true;
    ?EXITINGTAG -> true;
    ?RECVTAG -> true;
    ?URGENTTAG -> 
      case Arg of
	{_,Expr} -> isTagged(Expr)
      end;
    ?SYNCHTAG -> true;
    _ -> false
  end;
isTagged(_) -> false.

mkSynch(L) ->
  {Inputs,Outputs,Nondets} =
    lists:foldl
      (fun (Event,{In,Out,Nd}) ->
	   case Event of
	     {in,{Port,GuardFun,ExecFun}} ->
	       {add_event(Port,{GuardFun,ExecFun},In),Out,Nd};
	     {out,{Port,GuardFun,Value,ExecFun}} ->
	       case evaluate_guard(GuardFun) of
		 true ->
		   {In,add_event(Port,{Value,ExecFun},Out),Nd};
		 false ->
		   {In,Out,Nd}
	       end;
	     {nondet,{GuardFun,ExecFun}} ->
	       case evaluate_guard(GuardFun) of
		 true ->
		   {In,Out,[ExecFun|Nd]};
		 false ->
		   {In,Out,Nd}
	       end;
	     Other -> 
	       io:format("*** error: strange synchronisation ~p~n",[Event]),
	       throw(bad)
	   end
       end, {[],[],[]}, L),
  {?SYNCHTAG,{Inputs,Outputs,Nondets}}.

%% We should use general sorted list functions to do this
add_event(Port,Value,Events) ->
  add_event(Port,Value,Events,[]).

add_event(Port,Value,[],Seen) ->
  lists:reverse(Seen,[{Port,[Value]}]);
add_event(Port,Value,[{Port1,Values}|Rest],Seen) ->
  if
    Port == Port1 ->
      lists:reverse(Seen,[{Port1,[Value|Values]}|Rest]);
    Port > Port1 ->
      add_event(Port,Value,Rest,[{Port1,Values}|Seen]);
    true ->
      lists:reverse(Seen,[{Port,[Value]},{Port1,Values}|Rest])
  end.

evaluate_guard(GuardFun) ->
  try GuardFun() of
    Result ->
      if
	is_boolean(Result) -> Result;
	true ->
	  io:format
	    ("*** Error: guard ~p does not evaluate to a boolean but to ~p~n",
	     [GuardFun,Result]),
	  throw(bad)
      end
  catch Cause:Reason ->
      io:format
	("*** Error: evaluating guard function~n~p~nthrows an exception ~p:~p~n",
	 [GuardFun,Cause,Reason]),
      Trace = erlang:get_stacktrace(),
      io:format
	("Stack trace:\n"++(mce_erl_debugger:printStackTrace(2,Trace))++"\n"),
      throw(bad)
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
