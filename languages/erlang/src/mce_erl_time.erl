-module(mce_erl_time).

-export([now/0,nowRef/0,was/1,forget/1]).

now() ->
  State = mce_erl_state:getState(),
  mce_erl_state:getTime(State).

nowRef() -> 
  State = mce_erl_state:getState(),
  ClockId = mce_erl_references:getNewClock(State),
  mce_erl_state:setState(mce_erl_sysOS:addClock(ClockId,State)),
  ClockId.

was(ClockId) ->
  State = mce_erl_state:getState(),
  Clocks = mce_erl_state:getClocks(State),
  case mce_utils:find(fun ({ClockId2,_}) -> ClockId==ClockId2 end, Clocks) of
    {ok,{_,Time}} ->
      Time;
    _ ->
      throw(badarg)
  end.

forget(ClockId) ->
  State = mce_erl_state:getState(),
  mce_erl_state:setState(mce_erl_sysOS:removeClock(ClockId,State)).

  
  
  
