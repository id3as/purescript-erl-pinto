-module(pinto_timer@foreign).

-export([ cancel_/1,
          sendEvery_/3,
          sendAfter_/3 ]).

cancel_(Pid) ->
  fun() ->
      Pid ! stop,
      ok
  end.

%% Note: The actual timer implementation deliberately avoids using processes for
%% this sort of thing because it means needlessly exhauting the process pool
%% so a cleverer solution will probably be needed in time
sendEvery_(Wrapper, Milliseconds, Effect) ->
  Fun = fun Fun(MaybeRef) ->
            { ok, Ref } = case MaybeRef of
                            undefined -> timer:send_interval(Milliseconds, tick);
                            _ -> {  ok, MaybeRef }
                          end,
            receive
              stop ->
                io:format(user, "Shutting down timer cos parent died", []),
                timer:cancel(Ref);
              tick ->
                Effect(),
                Fun(Ref)
            end
        end,
  fun() -> Pid = spawn_link(fun() -> Fun(undefined) end), Wrapper(Pid) end.

sendAfter_(Wrapper, Milliseconds, Effect) ->
  _Parent = self(),
  Fun = fun Fun() ->
            { ok, Ref } = timer:send_after(Milliseconds, tick),
            receive
              stop ->
                timer:cancel(Ref);
              tick ->
                Effect();
              Other ->
                io:format(user, "Wtf ~p~n", [ Other])
            end
        end,
  fun() -> Pid = spawn_link(fun() -> Fun() end), Wrapper(Pid) end.
