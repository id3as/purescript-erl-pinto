-module(pinto_timer@foreign).

-export([ cancel/1,
          sendEveryFFI/3,
          sendAfterFFI/3 ]).

cancel(WrappedRef) ->
  fun() ->
      case WrappedRef of
        { timer, Ref } ->
          timer:cancel(Ref);
        { erlang,  Ref } ->
          erlang:cancel_timer(Ref)
      end
  end.

sendEveryFFI(Milliseconds,Msg, Target) ->
  fun() ->
    { ok, Ref } = timer:send_interval(Milliseconds, Target, Msg),
    { timer, Ref }
  end.

sendAfterFFI(Milliseconds, Msg, Target) ->
  fun() ->
    Ref = erlang:send_after(Milliseconds, Target, Msg),
    { erlang, Ref }
  end.
