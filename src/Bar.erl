-module(bar@foreign).

-export([ parseMonitorMsg/1
        , parseExitMsg/1
        , monitorImpl/1
        , demonitorImpl/1
        , milliseconds/0
        , size/1
        , fprofStart/0
        ]).


%%------------------------------------------------------------------------------
%% PS representation helpers
%%------------------------------------------------------------------------------
-define(left(X), {left, X}).
-define(right(X), {right, X}).


-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(exitMsg(Pid, Reason), {exitMsg, Pid, Reason}).
-define(kill, {kill}).
-define(normal, {normal}).
-define(other(X), {other, X}).




monitorImpl(Pid) ->
  fun() ->
    Ref = erlang:monitor(process, Pid),
    io:format(user, "Called monitor ~p ~p~n", [Ref, Pid]),
    Ref
  end.

demonitorImpl(Ref) ->
  fun() ->
    erlang:demonitor(Ref)
  end.


parseExitMsg(Msg) ->
    case Msg of
        {'EXIT', Pid, Reason} ->
            ?just(?exitMsg(Pid, exit_reason_to_ps(Reason)));
        _ ->
            ?nothing
    end.

exit_reason_to_ps(killed) -> ?kill;
exit_reason_to_ps(normal) -> ?normal;
exit_reason_to_ps(Other) -> ?other(Other).


-define(down(Ref, Type, Object, Info), {down, Ref, Type, Object, Info}).


parseMonitorMsg(Msg) ->
    case Msg of
        {'DOWN', MonitorRef, MonitorType, MonitorObject, MonitorInfo} ->
            ?just(?down(MonitorRef, MonitorType, MonitorObject, MonitorInfo));
        _ ->
            ?nothing
    end.

milliseconds() ->
  fun() ->
          erlang:system_time(millisecond)
  end.

size(X) ->
  fun() ->
      {tuple, erts_debug:size(X), erts_debug:flat_size(X) }
  end.

fprofStart() ->
  fun() ->
      ok = fprof:trace(verbose)
  end.

