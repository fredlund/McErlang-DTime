-module(snd_rcv).
-compile(export_all).

-include("mce_opts.hrl").
-include("stackEntry.hrl").

snd(Pid) ->
  Pid!hello,
  Pid!world,
  receive
    impossible -> snd(Pid)
  end.

rcv() ->
  receive
    _ -> rcv()
  end.

start() ->  
  Pid = spawn(fun () -> rcv() end),
  spawn(fun () -> snd(Pid) end),
  spawn(fun () -> clock(1) end),
  receive
    impossible -> start
  end.

clock(Tick) ->
  mce_erl:urgent(),
  receive
  after Tick ->
      mce_erl:urgent(),
      mce_erl:probe(tick),
      clock(Tick)
  end.
  
latest(_Tick,0,F) -> mce_erl:urgent(), F();
latest(Tick,Time,F) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), F() end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,F) end end]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mc() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      monitor=snd_rcv_mon,
      sends_are_sefs=true,
      shortest=true,
      save_table=true,
      discrete_time=true}).

dot() ->
  mc(),
  file:write_file
    ("hej.dot",
     mce_dot:from_table
     (mce_result:table(mce:result()),
      void,
      fun print_actions/1)).

dots() ->
  mc(),
  file:write_file
    ("hej.dot",
     mce_dot:from_stack
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

debug() ->
  mce:start
    (#mce_opts
     {program={?MODULE,start,[]},
      is_infinitely_fast=false,
      table=mce_table_hashWithActions,
      algorithm=mce_alg_debugger,
      sim_actions=true,
      sends_are_sefs=true,
      save_table=true,
      discrete_time=true}).




	    
	    
