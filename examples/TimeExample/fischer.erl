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
    0 -> set(Id,Tick,D,T);
    _ -> idle(Id,Tick,D,T)
  end.

set(Id,Tick,D,T) ->
  latest(Tick,D,{?MODULE,setting,[Id,Tick,D,T]}).

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
  write(0), 
  mce_erl:probe({exit,Id}), 
  idle(Id,Tick,D,T).

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

latest(_Tick,0,{M,F,Args}) ->
  mce_erl:urgent(), 
  apply(M,F,Args);
latest(Tick,Time,Fun={M,F,Args}) ->
  mce_erl:urgent(),
  mce_erl:choice
    ([fun () -> mce_erl:urgent(), apply(M,F,Args) end,
      fun () -> mce_erl:urgent(), receive after Tick -> latest(Tick,Time-Tick,Fun) end end]).


			    
		
	  
  

 
