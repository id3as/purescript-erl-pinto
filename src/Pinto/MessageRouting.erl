-module(pinto_messageRouting@foreign).

-export([ startRouterImpl/5
        , maybeStartRouterImpl/5
        , stopRouter/1
        , stopRouterFromCallback/0
        ]).

%% RegisterListener is of type Effect msg (so is effectively a function with no args)
%% DeregisterListener is of type (msg -> Effect Unit) and takes this  value and gives us an Effect
%% with which we will need to invoke manually here
startRouterImpl(Ref, RegisterListener, DeregisterListener, Callback, State) ->
  fun() ->
      {just, Result } = (maybeStartRouterImpl(Ref, fun() -> { just, RegisterListener() } end, DeregisterListener, Callback, State))(),
      Result
  end.

maybeStartRouterImpl(Ref, RegisterListener, DeregisterListener, Callback, State) ->
  Recipient = self(),
  Fun = fun Fun(Handle, MonitorRef, InnerState) ->
              receive
                {stop, From, StopRef} ->
                  (DeregisterListener(Handle))(),
                  demonitor(MonitorRef),
                  From ! {stopped, StopRef},
                  exit(normal);
                {'DOWN', MonitorRef, _, _, _} ->
                  (DeregisterListener(Handle))(),
                  exit(normal);
                Msg ->
                  InnerState2 = try
                    ((Callback(Msg))(InnerState))()
                  catch
                    Class:Reason:Stack ->
                      Recipient ! {error, {message_router_callback_failed, {Class, Reason, Stack}}},
                      exit(error)
                  end,
                  Fun(Handle, MonitorRef, InnerState2)
              end
           end,
  fun() ->
    {Pid, MonitorRef} = spawn_monitor(fun() ->
                                          MaybeHandle = RegisterListener(),
                                          case MaybeHandle of
                                            {just, Handle} ->
                                              Recipient ! { start_result, Handle },
                                              MonitorRef = monitor(process, Recipient),
                                              Fun(Handle, MonitorRef, State);
                                            {nothing} ->
                                              Recipient ! { start_result, undefined }
                                          end
                                      end),
    receive
      {'DOWN', MonitorRef, _, _, _} ->
        {nothing};
      { start_result, undefined } ->
        erlang:demonitor(MonitorRef, [flush]),
        {nothing};
      { start_result, Result } ->
        erlang:demonitor(MonitorRef, [flush]),
        {just, (Ref(Result))(Pid) }
    end
  end.

stopRouterFromCallback() ->
  Self = self(),
  fun() ->
      Ref = make_ref(),
      Self ! {stop, self(), Ref},
      ok
  end.

stopRouter({_, _, Pid}) ->
  fun() ->
      Ref = make_ref(),
      MRef = erlang:monitor(process, Pid),
      Pid ! {stop, self(), Ref},
      receive
        {stopped, Ref} -> ok;
        {'DOWN', MRef, _, _, _} -> ok
      end,
      erlang:demonitor(MRef, [flush]),
      ok
  end.
