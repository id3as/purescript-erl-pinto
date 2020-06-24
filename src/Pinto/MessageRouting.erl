-module(pinto_messageRouting@foreign).

-export([ startRouterImpl/4
        , maybeStartRouterImpl/4
        , stopRouter/1
        , stopRouterFromCallback/0
        ]).

%% RegisterListener is of type Effect msg (so is effectively a function with no args)
%% DeregisterListener is of type (msg -> Effect Unit) and takes this  value and gives us an Effect
%% with which we will need to invoke manually here
startRouterImpl(Ref, RegisterListener, DeregisterListener, Callback) ->
  Recipient = self(),
  Fun = fun Fun(Handle, MonitorRef) ->
              receive
                stop ->
                  (DeregisterListener(Handle))(),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', MonitorRef, _, _, _} ->
                  (DeregisterListener(Handle))(),
                  exit(normal);
                Msg ->
                  (Callback(Msg))(),
                  Fun(Handle, MonitorRef)
              end
           end,
  fun() ->
    Pid = spawn_link(fun() ->
                   Handle = RegisterListener(),
                   Recipient ! { start_result, Handle },
                   MonitorRef = monitor(process, Recipient),
                   Fun(Handle, MonitorRef)
               end),
    receive
      { start_result, Result } ->
        (Ref(Result))(Pid)
    end
  end.

maybeStartRouterImpl(Ref, RegisterListener, DeregisterListener, Callback) ->
  Recipient = self(),
  Fun = fun Fun(Handle, MonitorRef) ->
              receive
                stop ->
                  (DeregisterListener(Handle))(),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', MonitorRef, _, _, _} ->
                  (DeregisterListener(Handle))(),
                  exit(normal);
                Msg ->
                  (Callback(Msg))(),
                  Fun(Handle, MonitorRef)
              end
           end,
  fun() ->
    Pid = spawn_link(fun() ->
                   MaybeHandle = RegisterListener(),
                   case MaybeHandle of
                     {just, Handle} ->
                       Recipient ! { start_result, Handle },
                       MonitorRef = monitor(process, Recipient),
                       Fun(Handle, MonitorRef);
                     {nothing} ->
                       Recipient ! { start_result, undefined },
                       {nothing}
                   end
               end),
    receive
      { start_result, undefined } ->
        {nothing};
      { start_result, Result } ->
        {just, (Ref(Result))(Pid) }
    end
  end.

stopRouterFromCallback() ->
  Self = self(),
  fun() ->
      Self ! stop,
      ok
  end.

stopRouter({_, _, Pid}) ->
  fun() ->
      Pid ! stop,
      ok
  end.
