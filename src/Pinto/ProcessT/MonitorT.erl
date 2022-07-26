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

monitorImpl(Pid) ->
  fun() ->
    erlang:monitor(process, Pid)
  end.

demonitorImpl(Ref) ->
  fun() ->
    erlang:demonitor(Ref, [flush])
  end.


-define(down(Ref, Type, Object, Info), {down, Ref, Type, Object, Info}).


parseMonitorMsg(Msg) ->
    case Msg of
        {'DOWN', MonitorRef, MonitorType, MonitorObject, MonitorInfo} ->
            ?just(?down(MonitorRef, MonitorType, MonitorObject, MonitorInfo));
        _ ->
            ?nothing
    end.
