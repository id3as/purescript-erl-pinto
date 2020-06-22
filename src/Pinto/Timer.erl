-module(pinto_timer@foreign).

-export([ cancel_/1,
          sendEvery_/2,
          sendAfter_/2 ]).

cancel_(Ref) ->
  fun() ->
    timer:cancel(Ref)
  end.

%% Sod it, simple
sendEvery_(Milliseconds, Msg) ->
  fun() ->
    { ok, Ref } = timer:send_interval(Milliseconds, Msg),
    Ref
  end.

%% Ditto
sendAfter_(Milliseconds, Msg) ->
  fun() ->
    { ok, Ref } = timer:send_after(Milliseconds, Msg),
    Ref
  end.
