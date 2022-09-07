-module(pinto_processT_busT_stateBusT@foreign).

-export([ createImpl/3
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
-define(stateAttribute(Generation, State), {?stateTag, {Generation, State}}).
-define(initialStateMsg(Generation, State), {initialStateMsg, Generation, State}).
-record(dataMsg, {generation, msg}).

-define(msgTag, '__StateBusTMsg').

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
  gproc:send(?gprocPropertyKey(BusName), {?msgTag, BusName, Msg}).

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
      try
        GenAndState = gproc:get_attribute(?gprocNameKey(BusName), ?stateTag),
        %% self() ! snd GenAndState
        ?just(GenAndState)
      catch
        error:badarg ->
        ?nothing
      end
  end.

unsubscribeImpl(BusName) ->
  fun() ->
      gproc:unreg(?gprocPropertyKey(BusName)),
      ?unit
  end.

parseBusMsg(X) ->
  io:format(user, "Parse ~p~n", [X]),
  parseBusMsg_(X).
parseBusMsg_({?msgTag, Name, Msg}) ->
  io:format(user, "Matched~n", []),
  ?just({Name, Msg});
parseBusMsg_(_) ->
  ?nothing.
