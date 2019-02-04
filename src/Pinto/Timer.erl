-module(pinto_timer@foreign).

-export([ cancel_/1,
          sendEvery_/4,
          sendAfter_/4 ]).

cancel_(Pid) ->
  fun() ->
      Pid ! stop,
      ok
  end.

sendEvery_(Wrapper, _, Milliseconds, Fn) ->
  Parent = self(),
  Fun = fun Fun() ->
            { ok, Ref } = timer:send_interval(Milliseconds, tick),
            receive
              stop -> timer:cancel(Ref);
              tick ->
                Parent ! { routed_message, Fn },
                Fun()
            end
        end,
  fun() -> Pid = spawn_link(Fun), Wrapper(Pid) end.

sendAfter_(Wrapper, _, Milliseconds, Fn) ->
  Parent = self(),
  Fun = fun _Fun() ->
            { ok, Ref } = timer:send_after(Milliseconds, tick),
            receive
              stop -> timer:cancel(Ref);
              tick ->  Parent ! { routed_message, Fn }
            end
        end,
  fun() -> Pid = spawn_link(Fun), Wrapper(Pid) end.
