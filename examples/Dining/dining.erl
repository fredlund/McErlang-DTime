-module(dining).
-export([start/1]).

start(N) ->
  Ps = lists:map (fun (X) -> {X,X} end, lists:seq(1,N)),
  Forks = lists:map (fun (_) -> fork:start() end, Ps),
  lists:foreach
    (fun ({L,R}) -> philosopher:start(1, 1, L, R) end, adjacent(Forks)).
  
adjacent([])     -> [];
adjacent([X|Xs]) ->
  lists:zip([X] ++ Xs, Xs ++ [X]).
