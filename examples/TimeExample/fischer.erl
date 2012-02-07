-module(fischer).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Algorithm code

start(N,Tick,D,T) ->
  mcerlang:nput(id,0),
  lists:foreach
    (fun (Id) -> spawn(?MODULE,idle,[Id,Tick,D,T]) end,
     lists:seq(1,N)).

idle(Id,Tick,D,T) ->
  case read() of
    0 -> latest(Tick,D,fun () -> setting(Id,Tick,D,T) end);
    _ -> idle(Id,Tick,D,T)
  end.

setting(Id,Tick,D,T) ->
  write(Id),
  sleep(T),
  testing(Id,Tick,D,T).

testing(Id,Tick,D,T) ->
  case read() of
    Id -> mutex(Id,Tick,D,T);
    0 -> latest(Tick,D,fun () -> setting(Id,Tick,D,T) end);
    N -> idle(Id,Tick,D,T)
  end.
  
mutex(Id,Tick,D,T) ->
  mce_erl:probe({enter,Id}),
  mce_erl:pause
    (fun () ->
	 mce_erl:probe({exit,Id}), 
	 write(0), 
	 idle(Id,Tick,D,T)
     end).

read() ->
  case mcerlang:nget(id) of
    N when is_integer(N),N>=0 -> N
  end.

write(V) ->
  mcerlang:nput(id,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support code

sleep(Milliseconds) ->
  receive
  after Milliseconds -> ok
  end.

latest(_Tick,0,F) -> mce_erl:urgent(), F();
latest(Tick,Time,F) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), F() end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,F) end end]).


			    
		
	  
  

 
