-module(pinto_processT_busT_stateBusT@foreign).

-export([ create/2
        , deleteImpl/2
        , raiseImpl/3
        , subscribeImpl/1
        , unsubscribeImpl/1
        , parseBusMsg/1
        ]).

-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(unit, unit).

-define(stateBusTag, stateBusT).
-define(gprocPropertyKey(Name), {p,l,{?stateBusTag, Name}}).
-define(gprocNameKey(Name),     {n,l,{?stateBusTag, Name}}).

-define(stateTag, state).
-define(stateAttribute(Generation, State), {?stateTag, Generation, State}).
-define(initialStateMsg(Generation, State), {initialStateMsg, Generation, State}).
-record(dataMsg, {generation :: non_neg_integer(), msg :: term()}).

-define(msgTag, '__StateBusTMsg').

create(BusName, InitialState) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      gproc:reg(NameKey, undefined, [?stateAttribute(0, InitialState)]),
      raiseMsgInt(BusName, ?initialStateMsg(0, InitialState)),
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
  gproc:send(?gprocPropertyKey(BusName), {?msgTag, BusName, Msg}).

raiseImpl(Updater, BusName, Msg) ->
  fun() ->
    NameKey = ?gprocNameKey(BusName),
    ?stateAttribute(Generation, State) = gproc:get_attribute(NameKey, ?stateTag),
    NewGeneration = Generation + 1,
    NewState = Updater(Msg, State),
    gproc:set_attributes(NameKey, ?stateAttribute(NewGeneration, NewState)),
    raiseMsgInt(BusName, #dataMsg{generation = NewGeneration, msg = Msg})
  end.

subscribeImpl(BusName) ->
  fun() ->
      true = gproc:reg(?gprocNameKey(BusName)),
      ?unit
  end.

unsubscribeImpl(BusName) ->
  fun() ->
      gproc:unreg(?gprocNameKey(BusName)),
      ?unit
  end.

parseBusMsg({?msgTag, Name, Msg}) ->
  ?just({Name, Msg});
parseBusMsg(_) ->
  ?nothing.
