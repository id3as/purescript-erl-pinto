-module(bar@foreign).

-export([ foo/1
        , bar/1
        , parseMonitorMsg/1
        , parseExitMsg/1
        , monitorImpl/1
        , demonitorImpl/1
        ]).


parseMonitorMsg(_) -> 1.
parseExitMsg(_) -> 1.
%%------------------------------------------------------------------------------
%% PS representation helpers
%%------------------------------------------------------------------------------
-define(left(X), {left, X}).
-define(right(X), {right, X}).

-define(exitMsg(Pid, Reason), {exitMsg, Pid, Reason}).
-define(kill, {kill}).
-define(normal, {normal}).
-define(other(X), {other, X}).




monitorImpl(Pid) ->
  fun() ->
    erlang:monitor(process, Pid)
  end.

demonitorImpl(Ref) ->
  fun() ->
    erlang:demonitor(Ref)
  end.


foo(Msg) ->
    case Msg of
        {'EXIT', Pid, Reason} ->
            ?left(?exitMsg(Pid, exit_reason_to_ps(Reason)));
        _ ->
            ?right(Msg)
    end.

exit_reason_to_ps(killed) -> ?kill;
exit_reason_to_ps(normal) -> ?normal;
exit_reason_to_ps(Other) -> ?other(Other).


-define(down(Ref, Type, Object, Info), {down, Ref, Type, Object, Info}).


bar_receive() ->
    receive
        X -> bar(X)
    end.

bar(Msg) ->
    case Msg of
        {'DOWN', MonitorRef, MonitorType, MonitorObject, MonitorInfo} ->
            ?left(?down(MonitorRef, MonitorType, MonitorObject, MonitorInfo));
        _ ->
            ?right(Msg)
    end.
