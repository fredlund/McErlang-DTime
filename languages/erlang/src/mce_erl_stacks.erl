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

-export([mkSend/2,mkLet/2,mkTry/3,tryValue/3,tryHandler/2,parseStack/1]).
-export([execStack/2,isTagged/1]).
-export([mkUrgent/1,mkSlow/1]).
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
  mce_erl:urgent(Cont).

mkSlow(Cont) ->
  mce_erl:slow(Cont).

tryValue(Value, BodyCont, HandlerCont) ->
    case isTagged(Value) of
      false -> apply_value(BodyCont, Value);
      true -> mce_erl:tryexpr(Value, BodyCont, HandlerCont)
    end.

tryHandler(Value,HandlerCont) ->
  apply_value(HandlerCont,Value).

apply_value(Cont,Value) ->
  case Cont of
    {Module,Fun,Args} ->
      apply(Module,Fun,[Value|Args]);
    {Fun,Args} ->
      apply(Fun,[Value|Args])
  end.

parseStack(Context) ->
  parseStack(Context,[]).
parseStack(Entry={?RECVTAG,_},RestStack) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Entry={?CHOICETAG,_},RestStack) ->
  {Entry,lists:reverse(RestStack)};
parseStack(Entry={?SENDTAG,_},RestStack) ->
  {Entry,lists:reverse(RestStack)};
parseStack({?LETTAG,{Expr,Cont}},RestStack) ->
  parseStack(Expr,[{?LETTAG,{void,Cont}}|RestStack]);
parseStack({?URGENTTAG,Expr},RestStack) ->
  parseStack(Expr,[{?URGENTTAG,void}|RestStack]);
parseStack({?SLOWTAG,Expr},RestStack) ->
  parseStack(Expr,[{?SLOWTAG,void}|RestStack]);
parseStack({?TRYTAG,{Expr,Cont}},RestStack) ->
  parseStack(Expr,[{?TRYTAG,{void,Cont}}|RestStack]);
parseStack(Arg1,Arg2) ->
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
	("*** Error: malformed command~n~p~nin execStack nwith empty context~n",
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
execStack(Command,[{?URGENTTAG,_}|Rest]) ->
  execStack(Command,Rest);
execStack(Command,[{?SLOWTAG,_}|Rest]) ->
  execStack(Command,Rest);
execStack(Command,OtherTag) ->
  io:format
    ("*** Error: malformed tag in execStack:~n~p~nfor command~n~p~n",
     [OtherTag,Command]),
  throw(bad).
  
isTagged({MaybeTag,_}) ->
  case MaybeTag of
    ?TRYTAG -> true;
    ?LETTAG -> true;
    ?CHOICETAG -> true;
    ?SENDTAG -> true;
    ?EXITINGTAG -> true;
    ?RECVTAG -> true;
    ?URGENTTAG -> true;
    ?SLOWTAG -> true;
    _ -> false
  end;
isTagged(_) -> false.


    

