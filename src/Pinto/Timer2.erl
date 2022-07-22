-module(pinto_timer2@foreign).

-export([ cancel/1,
          sendEveryToFFI/3,
          sendAfterToFFI/3 ]).

cancel(WrappedRef) ->
  fun() ->
      case WrappedRef of
        { timer, Ref } ->
          timer:cancel(Ref);
        { erlang,  Ref } ->
          erlang:cancel_timer(Ref)
      end
  end.

sendEveryToFFI(Milliseconds,Msg, Target) ->
  fun() ->
    { ok, Ref } = timer:send_interval(round(Milliseconds), Target, Msg),
    { timer, Ref }
  end.

sendAfterToFFI(Milliseconds, Msg, Target) ->
  fun() ->
    Ref = erlang:send_after(round(Milliseconds), Target, Msg),
    { erlang, Ref }
  end.
