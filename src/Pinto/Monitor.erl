-module(pinto_monitor@foreign).

-export([ startMonitor/1
        , stopMonitor/1
        , handleMonitorMessage/3
        ]).

startMonitor(Pid) ->
  fun() ->
    erlang:monitor(process, Pid)
  end.

stopMonitor(Ref) ->
  fun() ->
    erlang:demonitor(Ref)
  end.

%% Note: The client of this monitor code gets a RouterRef
%% which is just a new typed pid of the worker process in the message router
%% so 'self' suffices for identifying which monitor is invoking the callback
%% although given the presence of the callback, it would probably be more prudent
%% to simply load context into that as a partially applied fn
handleMonitorMessage(Wrapper, Callback, {'DOWN', _MonitorRef, MonitorType, MonitorObject, MonitorInfo}) ->
  fun() ->
      Msg = (((Wrapper(self()))({MonitorType}))(MonitorObject))(MonitorInfo),
      (Callback(Msg))()
  end.
