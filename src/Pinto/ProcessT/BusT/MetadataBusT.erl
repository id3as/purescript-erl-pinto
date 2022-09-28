-module(pinto_processT_busT_metadataBusT@foreign).

-export([ createImpl/3
        , deleteImpl/2
        , raiseImpl/2
        , updateMetadataImpl/2
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

-define(metadataBusTag, metadataBusT).
-define(gprocPropertyKey(Name), {p,l,{?metadataBusTag, Name}}).
-define(gprocNameKey(Name),     {n,l,{?metadataBusTag, Name}}).

-define(metadataTag, metadata).
-define(metadataAttribute(Generation, Metadata), {?metadataTag, {Generation, Metadata}}).
-record(dataMsgInternal, {generation, msg}).
-record(metadataMsgInternal, {generation, msg}).

-define(msgTag, '__MetadataBusTMsg').
-define(monitorTag, '__MetadataBusTMonitor').

createImpl(BusName, Generation, InitialMetadata) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      gproc:reg(NameKey, undefined, [?metadataAttribute(Generation, InitialMetadata)]),
      raiseMsgInt(BusName, #metadataMsgInternal{generation = Generation, msg = InitialMetadata}),
      BusName
  end.

deleteImpl(BusName, TerminatedMsg) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      ?metadataAttribute(Generation, _Metadata) = gproc:get_attribute(NameKey, ?metadataTag),
      gproc:unreg(?gprocNameKey(BusName)),
      raiseMsgInt(BusName, TerminatedMsg(Generation)),
      ?unit
  end.

raiseMsgInt(BusName, Msg) ->
  gproc:send(?gprocPropertyKey(BusName), {?msgTag, BusName, Msg, self()}).

raiseImpl(BusName, Msg) ->
  fun() ->
    NameKey = ?gprocNameKey(BusName),
    {{Id, Generation}, _} = gproc:get_attribute(NameKey, ?metadataTag),
    raiseMsgInt(BusName, #dataMsgInternal{generation = {Id, Generation}, msg = Msg})
  end.

updateMetadataImpl(BusName, NewMetadata) ->
  fun () ->
    NameKey = ?gprocNameKey(BusName),
    {{Id, Generation}, _} = gproc:get_attribute(NameKey, ?metadataTag),
    NewGeneration = {Id, Generation + 1},
    gproc:set_attributes(NameKey, [?metadataAttribute(NewGeneration, NewMetadata)]),
    raiseMsgInt(BusName, #metadataMsgInternal{generation = NewGeneration, msg = NewMetadata})
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
            {Gen, Metadata} = gproc:get_attribute(?gprocNameKey(BusName), Pid, ?metadataTag),
            ?just({Gen, Metadata, Ref})
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
