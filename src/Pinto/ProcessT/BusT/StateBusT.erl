-module(pinto_processT_busT_stateBusT@foreign).

-export([ createImpl/3
        , deleteImpl/2
        , raiseImpl/3
        , subscribeImpl/1
        , monitorImpl/2
        , unsubscribeImpl/2
        , parseBusMsg/1
        ]).

-define(left(X), {left, X}).
-define(right(X), {right, X}).
-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(unit, unit).

-define(stateBusTag, stateBusT).
-define(gprocPropertyKey(Name), {p,l,{?stateBusTag, Name}}).
-define(gprocNameKey(Name),     {n,l,{?stateBusTag, Name}}).

-define(stateTag, state).
-define(stateAttribute(Generation, State), {?stateTag, {Generation, State}}).
-define(initialStateMsg(Generation, State), {initialStateMsg, Generation, State}).
-record(dataMsg, {generation, msg}).

-define(msgTag, '__StateBusTMsg').
-define(monitorTag, '__StateBusTMonitor').

createImpl(BusName, Generation, InitialState) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      gproc:reg(NameKey, undefined, [?stateAttribute(Generation, InitialState)]),
      raiseMsgInt(BusName, ?initialStateMsg(Generation, InitialState)),
      BusName
  end.

deleteImpl(BusName, TerminatedMsg) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      ?stateAttribute(Generation, _State) = gproc:get_attribute(NameKey, ?stateTag),
      gproc:unreg(?gprocNameKey(BusName)),
      raiseMsgInt(BusName, TerminatedMsg(Generation)),
      ?unit
  end.

raiseMsgInt(BusName, Msg) ->
  gproc:send(?gprocPropertyKey(BusName), {?msgTag, BusName, Msg, self()}).

raiseImpl(Updater, BusName, Msg) ->
  fun() ->
    NameKey = ?gprocNameKey(BusName),
    {{Id, Generation}, State} = gproc:get_attribute(NameKey, ?stateTag),
    NewGeneration = {Id, Generation + 1},
    NewState = (Updater(Msg))(State),
    gproc:set_attributes(NameKey, [?stateAttribute(NewGeneration, NewState)]),
    raiseMsgInt(BusName, #dataMsg{generation = NewGeneration, msg = Msg})
  end.

subscribeImpl(BusName) ->
  fun() ->
      true = gproc:reg(?gprocPropertyKey(BusName)),
      Pid = gproc:where(?gprocNameKey(BusName)),
      case Pid of
        undefined -> ?nothing;
        _ ->
          Ref = (monitorImpl(Pid, BusName))(),
          try
            % For verifying this try/catch is necessary, see Test.StateBusT.testErrorHandling
            % timer:sleep(10),
            {Gen, State} = gproc:get_attribute(?gprocNameKey(BusName), Pid, ?stateTag),
            ?just({Gen, State, Ref})
          catch
            error:badarg ->
              erlang:demonitor(Ref, [flush]),
              ?nothing
          end
      end
  end.

monitorImpl(Pid, BusName) ->
  fun () -> erlang:monitor(process, Pid, [{tag, {?monitorTag, BusName}}]) end.


% sender exits
% immediately calling unsubscribe from other thread
% unreg message not even on the queue yet, would be received later
% causing conflicts if there's like a GprocT also receiving those kinds of message
% (if we delete the Key from the map)
unsubscribeImpl(MaybeRef, BusName) ->
  fun() ->
      case MaybeRef of
        ?just(Ref) -> erlang:demonitor(Ref, [flush]);
        ?nothing -> ok
      end,
      gproc:unreg(?gprocPropertyKey(BusName)),
      ?unit
  end.

parseBusMsg({?msgTag, Name, Msg, Pid}) ->
  ?just(?left({Name, Msg, Pid}));
parseBusMsg({{?monitorTag, BusName}, _MonitorRef, _MonitorType, _MonitorObject, _MonitorInfo}) ->
  ?just(?right(BusName));
parseBusMsg(_) ->
  ?nothing.
