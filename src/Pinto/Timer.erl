-module(pinto_timer@foreign).

-export([ cancel/1,
          sendEvery/3,
          sendAfter/3 ]).

cancel(WrappedRef) ->
  fun() ->
      case WrappedRef of
        { timer, Ref } ->
          timer:cancel(Ref);
        { erlang,  Ref } ->
          erlang:cancel_timer(Ref)
      end
  end.

sendEvery(Milliseconds,Msg, Target) ->
  fun() ->
    { ok, Ref } = timer:send_interval(Milliseconds, Target, Msg),
    { timer, Ref }
  end.

sendAfter(Milliseconds, Msg, Target) ->
  fun() ->
    Ref = erlang:send_after(Milliseconds, Target, Msg),
    { erlang, Ref }
  end.
