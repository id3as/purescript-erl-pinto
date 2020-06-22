-module(pinto_messageRouting@foreign).

-export([ startRouter/3
        , stopRouter/1
        , stopRouterFromCallback/0
        ]).

%% RegisterListener is of type Effect msg (so is effectively a function with no args)
%% DeregisterListener is of type (msg -> Effect Unit) and takes this  value and gives us an Effect
%% with which we will need to invoke manually here
startRouter(RegisterListener, DeregisterListener, Callback) ->
  Recipient = self(),
  Fun = fun Fun(Handle, MonitorRef) ->
              receive
                stop ->
                  (DeregisterListener(Handle))(),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', _, _, _, _} ->
                  (DeregisterListener(Handle))(),
                  exit(normal);
                Msg ->
                  (Callback(Msg))(),
                  Fun(Handle, MonitorRef)
              end
           end,
  fun() ->
    spawn_link(fun() ->
                   Handle = RegisterListener(),
                   MonitorRef = monitor(process, Recipient),
                   Fun(Handle, MonitorRef)
               end)
  end.

stopRouterFromCallback() ->
  Self = self(),
  fun() ->
      Self ! stop,
      ok
  end.

stopRouter(Ref) ->
  fun() ->
      Ref ! stop,
      ok
  end.
