-module(philosopher_synch).

-export([start/4]).

start(Think, Eat, Left, Right) ->
  Philospher = spawn(fun() -> loop(Think, Eat, Left, Right) end),
  Philospher.

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
  mce_erl:synch
    ([{out,
       {{grab,Fork},
	fun () -> true end,
	self(),
	fun () ->
	    mce_erl:synch
	      ([{in,
		 {fork,
		  fun (X) -> true end,
		  fun (X) -> fork end}}])
	end}}]).

release_forks([]) -> ok;
release_forks([Fork|Forks]) ->
  mce_erl:synch
    ([{out,
       {{release,Fork},
	fun () -> true end,
	self(),
	fun () -> release_forks(Forks) end}}]).

think(Time) ->
  %%io:format("~p thinking for ~p ms\n", [self(), Time]),
  %% timer:sleep(Time).
  ok.

eat(Time) ->
  %%io:format("~p eating for ~p ms\n", [self(), Time]),
  %% timer:sleep(Time).
  ok.

