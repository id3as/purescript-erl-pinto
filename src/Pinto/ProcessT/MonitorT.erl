-module(pinto_processT_monitorT@foreign).

-export([ parseMonitorMsg/1
        , monitorImpl/1
        , demonitorImpl/1
        ]).


%%------------------------------------------------------------------------------
%% PS representation helpers
%%------------------------------------------------------------------------------
-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(down(Ref, Type, Object, Info), {down, Ref, Type, Object, Info}).
-define(monitorTag, 'MonitorT').

monitorImpl(Pid) ->
  fun() ->
    erlang:monitor(process, Pid, [{tag, ?monitorTag}])
  end.

demonitorImpl(Ref) ->
  fun() ->
    erlang:demonitor(Ref, [flush])
  end.

parseMonitorMsg(Msg) ->
    case Msg of
        {?monitorTag, MonitorRef, MonitorType, MonitorObject, MonitorInfo} ->
            ?just(?down(MonitorRef, MonitorType, MonitorObject, MonitorInfo));
        _ ->
            ?nothing
    end.
