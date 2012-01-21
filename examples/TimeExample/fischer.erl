-module(fischer).
-compile(export_all).

-include("mce_opts.hrl").
-include("stackEntry.hrl").

-behaviour(mce_behav_monitor).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Algorithm code

start(N,Tick,D,T) ->
  lists:foreach
    (fun (Id) -> spawn(?MODULE,idle,[Id,Tick,D,T]) end,
     lists:seq(1,N)).

idle(Id,Tick,D,T) ->
  case read() of
    0 -> latest(Tick,D, fun () -> setting(Id,Tick,D,T) end);
    _ -> idle(Id,Tick,D,T) %% A time lock
  end.

setting(Id,Tick,D,T) ->
  write(Id),
  sleep(T),
  testing(Id,Tick,D,T).

testing(Id,Tick,D,T) ->
  case read() of
    Id -> mutex(Id,Tick,D,T);
    _ -> idle(Id,Tick,D,T)
  end.
  
mutex(Id,Tick,D,T) ->
  mce_erl:probe({enter,Id}),
  mce_erl:pause(fun () -> sleep(D), mce_erl:probe({exit,Id}), write(0), idle(Id,Tick,D,T) end).

read() ->
  case mcerlang:nget(id) of
    undefined ->
      0;
    N when is_integer(N),N>=0 ->
      N
  end.

write(V) ->
  mcerlang:nput(id,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support code

sleep(Milliseconds) ->
  receive
  after Milliseconds -> ok
  end.

latest(Tick,0,F) -> mce_erl:urgent(), F();
latest(Tick,Time,F) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), F() end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,F) end end]).

			    
		
	  
  

 
