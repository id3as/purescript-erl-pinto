-module(pinto_timer@foreign).

-export([ cancel_/1,
          sendEvery_/3,
          sendAfter_/3 ]).

cancel_(Ref) ->
  fun() ->
    timer:cancel(Ref)
  end.

%% Sod it, simple
sendEvery_(Wrapper, Milliseconds, Msg) ->
  fun() ->
    { ok, Ref } = timer:send_interval(Milliseconds, Msg),
    Wrapper(Ref)
  end.

%% Ditto
sendAfter_(Wrapper, Milliseconds, Msg) ->
  fun() ->
    { ok, Ref } = timer:send_after(Milliseconds, Msg),
    Wrapper(Ref)
  end.
