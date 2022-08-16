-module(test_busT@foreign).

-export([ sendSelfLateMessage/3
        ]).

-define(msgTag, '__BusTMsg').

sendSelfLateMessage(BusName, Msg, After) ->
  fun() ->
      Self = self(),
      spawn(fun() ->
              timer:sleep(trunc(After)),
              Self ! {?msgTag, BusName, Msg}
            end),
      unit
  end.
