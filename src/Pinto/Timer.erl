-module(pinto_timer@foreign).

-export([ cancel_/1,
          sendEvery_/3,
          sendAfter_/3 ]).

cancel_(Pid) ->
  fun() ->
      Pid ! stop,
      ok
  end.

sendEvery_(Wrapper, Milliseconds, Effect) ->
  Parent = self(),
  Fun = fun Fun(MaybeMonitorRef) ->
            MonitorRef = case MaybeMonitorRef of
                           undefined ->
                             monitor(process, Parent);
                           _ -> MaybeMonitorRef
                         end,
            { ok, Ref } = timer:send_interval(Milliseconds, tick),
            receive
              stop ->
                demonitor(MonitorRef),
                timer:cancel(Ref);
              {'DOWN', _, _, _, _} ->
                timer:cancel(Ref),
                ok;
              tick ->
                Effect(),
                Fun(MonitorRef)
            end
        end,
  fun() -> Pid = spawn_link(fun() -> Fun(undefined) end), Wrapper(Pid) end.

sendAfter_(Wrapper, Milliseconds, Fn) ->
  Parent = self(),
  Fun = fun Fun(MaybeMonitorRef) ->
            MonitorRef = monitor(process, Parent),
            { ok, Ref } = timer:send_after(Milliseconds, tick),
            receive
              stop ->
                demonitor(MonitorRef),
                timer:cancel(Ref);
              {'DOWN', _, _, _, _} ->
                timer:cancel(Ref),
                ok;
              tick ->
                demonitor(MonitorRef),
                Effect(),
            end
        end,
  fun() -> Pid = spawn_link(fun() -> Fun(undefined) end), Wrapper(Pid) end.
