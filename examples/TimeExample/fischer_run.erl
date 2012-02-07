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
      shortest=true,
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

mc2(N,Tick,D,T) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      shortest=true,
      well_behaved=true,
      partial_order=true,
      monitor={?MODULE,void},
      save_table=true,
      discrete_time=true}).

mc2b(N,Tick,D,T) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=false,
      table={mce_table_bitHash,[10000000]},
      sends_are_sefs=true,
      well_behaved=true,
      partial_order=true,
      monitor={?MODULE,void},
      save_table=true,
      discrete_time=true}).

dot2(N,Tick,D,T) ->
  mc2(N,Tick,D,T),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

dot_space(N,Tick,D,T) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

dot_space2(N,Tick,D,T) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      sends_are_sefs=true,
      well_behaved=true,
      partial_order=true,
      save_table=true,
      discrete_time=true}),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

dots(N,Tick,D,T) ->
  mc(N,Tick,D,T),
  file:write_file
    ("hej.dot",
     mce_dot:from_stack
     (mce_result:stack(mce:result()),
      void,
      fun print_actions/1)).

dots2(N,Tick,D,T) ->
  mc2(N,Tick,D,T),
  file:write_file
    ("hej.dot",
     mce_dot:from_stack
     (mce_result:stack(mce:result()),
      void,
      fun print_actions/1)).

print_states() ->
  Result = mce:result(),
  Table = mce_result:table(Result),
  {ok, States} = mce_behav_tableOps:states_to_list(Table),
  lists:foreach(fun (State) -> io:format("~p~n",[State]) end, States).

print_actions(Actions) ->
  "label=\""++
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
     end, "", Actions)++
    "\"".

print_action(Action) ->
  Source =
    case mce_erl_actions:get_source(Action) of
      {pid,_,Pid} -> io_lib:format("~p",[Pid]);
      Other -> io_lib:format("~p",[Other])
    end,
  case mce_erl_actions:is_probe(Action) of
    true ->
      io_lib:format("~p",[mce_erl_actions:get_probe_label(Action)]);
    false ->
      case mce_erl_actions:is_send(Action) of
	true -> 
	  io_lib:format
	    ("~s: sent ~p",
	     [Source,mce_erl_actions:get_send_msg(Action)]);
	false ->
	  case mce_erl_actions:is_api_call(Action) of
	    true ->
	      io_lib:format
		("~s: ~p(~p) -> ~p",
		 [Source,
		  mce_erl_actions:get_api_call_fun(Action),
		  mce_erl_actions:get_api_call_arguments(Action),
		  mce_erl_actions:get_api_call_result(Action)]);
	    false ->
	      case mce_erl_actions:is_timeout(Action) of
		true ->
		  case mce_erl_actions:get_timeout(Action) of
		    Tick={_,_,_} ->
		      io_lib:format("~s: timeout ~p",[Source,Tick]);
		    _ ->
		      io_lib:format("~s: timeout",[Source])
		  end;
		false ->
		  case mce_erl_actions:get_name(Action) of
		    run -> "";
		    Name -> io_lib:format("~s: ~p",[Source,Name])
		  end
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

debug2(N,Tick,D,T) when N>0, is_integer(N) ->
  mce:start
    (#mce_opts
     {program={fischer,start,[N,Tick,D,T]},
      is_infinitely_fast=true,
      table=mce_table_hashWithActions,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      well_behaved=true,
      partial_order=true,
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
    (fun (Action,false) ->
	 mce_erl_actions:is_probe(Action) andalso
	   case mce_erl_actions:get_probe_label(Action) of
	     {Tag,Id} -> {true,Id};
	     _ -> false
	   end;
	 (_Action,Other) -> Other
     end, false, Actions).




			    
		
	  
  

 
