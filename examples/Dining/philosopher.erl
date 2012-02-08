-module(philosopher).

-export([start/4,stop/1]).

start(Think, Eat, Left, Right) ->
  Philospher = spawn(fun() -> loop(Think, Eat, Left, Right) end),
  Philospher.

stop(Pid) ->
  Pid ! {stop, self()},
  receive
    ok -> ok
  end.

loop(Think, Eat, Left, Right) ->
  think(Think),
  MaybeSorted =
    lists:sort([Left,Right]),
    %% [Left,Right],
  grab_forks(MaybeSorted),
  eat(Eat),
  release_forks([Left, Right]),
  loop(Think, Eat, Left, Right).

grab_forks([]) -> ok;
grab_forks([F|Forks]) ->
  %%io:format("Forks: ~p~n",[Forks]),
  grab_fork(F), grab_forks(Forks).

grab_fork(Fork) ->
  %%io:format("grabbing fork ~p~n",[Fork]),
  Fork ! {grab, self()},
  receive
    fork -> 
      %%io:format("got fork ~p~n",[Fork]), 
      fork
  end.

release_forks([]) -> ok;
release_forks([Fork|Forks]) ->
  Fork ! {release, self()}, 
  release_forks(Forks).

think(Time) ->
  %%io:format("~p thinking for ~p ms\n", [self(), Time]),
  %% timer:sleep(Time).
  ok.

eat(Time) ->
  %%io:format("~p eating for ~p ms\n", [self(), Time]),
  %% timer:sleep(Time).
  ok.

