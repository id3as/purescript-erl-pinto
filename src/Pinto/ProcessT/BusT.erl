-module(pinto_processT_busT@foreign).

-export([ parseBusMsg/1
        , raise/2
        , subscribeImpl/1
        , unsubscribeImpl/1
        ]).



-define(key(Name), {p,l, {sb, Name}}).

%%------------------------------------------------------------------------------
%% PS representation helpers
%%------------------------------------------------------------------------------
-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(unit, unit).
-define(msgTag, '__BusTMsg').


raise(BusName, Msg) ->
  fun() ->
    gproc:send(?key(BusName), {?msgTag, BusName, Msg}),
    ?unit
  end.

subscribeImpl(BusName) ->
  fun() ->
      true = gproc:reg(?key(BusName)),
      ?unit
  end.

unsubscribeImpl(BusName) ->
  fun() ->
      gproc:unreg(?key(BusName)),
      ?unit
  end.

parseBusMsg({?msgTag, Name, Msg}) ->
  ?just({Name, Msg});
parseBusMsg(_) ->
  ?nothing.
