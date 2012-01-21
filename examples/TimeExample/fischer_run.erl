-module(fischer_run).
-language(erlang).
-compile(export_all).

-include("mce_opts.hrl").
-include("stackEntry.hrl").

-behaviour(mce_behav_monitor).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Modelchecking code:

mc(N,Tick,D,T) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      monitor={?MODULE,void},
      save_table=true,
      discrete_time=true}).

dot(N,Tick,D,T) ->
  mc(N,Tick,D,T),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

print_actions(Actions) ->
  "label=\""++
  lists:foldr
    (fun (Action,Rest) ->
	 if 
	   Rest=="" ->
	     io_lib:format("~s",[print_action(Action)]);
	   true ->
	     io_lib:format("~s,~s",[Rest,print_action(Action)])
	 end
     end, "", Actions)++
    "\"".

print_action(Action) ->
  case mce_erl_actions:is_probe(Action) of
    true ->
      io_lib:format("~p",[mce_erl_actions:get_probe_label(Action)]);
    false ->
      case mce_erl_actions:is_send(Action) of
	true -> 
	  io_lib:format("sent ~p",[mce_erl_actions:get_send_msg(Action)]);
	false ->
	  case mce_erl_actions:is_api_call(Action) of
	    true ->
	      io_lib:format
		("~p(~p) -> ~p",
		 [mce_erl_actions:get_api_call_fun(Action),
		  mce_erl_actions:get_api_call_arguments(Action),
		  mce_erl_actions:get_api_call_result(Action)]);
	    false ->
	      case mce_erl_actions:get_name(Action) of
		run -> "";
		Name -> io_lib:format("~p",[Name])
	      end
	  end
      end
  end.

debug(N,Tick,D,T) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      sends_are_sefs=true,
      monitor={?MODULE,void},
      save_table=true,
      discrete_time=true}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monitor code:

init(_) -> {ok,outside}.

stateChange(_,outside,Stack) ->
  Actions = actions(Stack),
  case has_enter(Actions) of
    {true,Id} -> {ok,{entered,Id}};
    false -> 
      case has_exit(Actions) of
	{true,Id} -> {outside_exit,Id};
	false -> {ok,outside}
      end
  end;
stateChange(_,{entered,Id},Stack) ->
  Actions = actions(Stack),
  case has_exit(Actions) of
    {true,Id} -> {ok,outside};
    {true,Id2} -> {other_exit,Id2,entered,Id};
    false -> 
      case has_enter(Actions) of
	{true,Id2} -> {no_mutex,Id,Id2};
	false -> {ok,{entered,Id}}
      end
  end.

monitorType() -> safety.

actions(Stack) ->
  {Element, _} = mce_behav_stackOps:pop(Stack),
  Element#stackEntry.actions.

has_enter(Actions) ->
  has_probe_with_tag(enter,Actions).

has_exit(Actions) ->
  has_probe_with_tag(exit,Actions).

has_probe_with_tag(Tag,Actions) ->
  lists:foldl
    (fun (Action,Acc) ->
	 Acc orelse
	   (mce_erl_actions:is_probe(Action) andalso
	    case mce_erl_actions:get_probe_label(Action) of
	      Label={Tag,Id} -> {true,Id};
	      _ -> false
	    end)
     end, false, Actions).


			    
		
	  
  

 
