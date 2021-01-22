-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

%% FFI Exports
-export([ startLinkFFI/2
        , selfFFI/0
        , callFFI/2
        , castFFI/2
        , mkReply/1
        ]).

%% gen_statem Exports
-export([ callback_mode/0
        , init/1
        , handle_event/4
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        , registry_name_from_ps/1
        , instance_name_from_ps/1
        ]).

%%% ----------------------------------------------------------------------------
%%% Directly Exported FFI
%%% ----------------------------------------------------------------------------
mkReply(To) ->
  fun(Reply) ->
      {reply, To, Reply}
  end.

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startLinkFFI(MaybeName, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            io:format(user, "Starting GenStatem without a name~n", []),
            gen_statem:start_link(?MODULE, InitEffect, []);
          {just, NamePS} ->
            Name = registry_name_from_ps(NamePS),
            io:format(user, "Starting GenStatem with name ~p~n", [Name]),
            gen_statem:start_link(Name, ?MODULE, InitEffect, [])
        end,

      start_link_result_to_ps(Result)
  end.

callFFI(StatemRef, CallFn) ->
  fun() ->
      io:format(user, "Issuing call to ~p~n", [statem_ref_from_ps(StatemRef)]),
      gen_server:call(statem_ref_from_ps(StatemRef), CallFn)
  end.

castFFI(StatemRef, CastFn) ->
  fun() ->
      io:format(user, "Issuing cast to ~p~n", [statem_ref_from_ps(StatemRef)]),
      ok = gen_server:cast(statem_ref_from_ps(StatemRef), CastFn),
      unit
  end.

selfFFI() ->
  fun() ->
      self()
  end.

%%% ----------------------------------------------------------------------------
%%% gen_statem API
%%% ----------------------------------------------------------------------------
callback_mode() ->
  [ handle_event_function, state_enter ].

init(InitEffect) ->

  io:format(user, "About to run init effect ~p~n", [InitEffect]),

  InitResult = InitEffect(),
  io:format(user, "InitResult ~p~n", [InitResult]),

  case InitResult of
    {outerInitOk, State, Data} -> {ok, State, Data};
    {outerInitOkWithActions, State, Data, Actions} -> {ok, State, Data, init_actions(Actions)};
    {outerInitStop, Error} -> {stop, Error};
    {outerInitIgnore} -> ignore
  end.

handle_event(enter, OldState, NewState, #{ handleEnter := HandleEnter } = Data) ->
  io:format(user, "Got state enter ~p -> ~p (~p)~n", [OldState, NewState, Data]),
  HandleEnterEffect = HandleEnter(OldState, NewState, Data),
  HandleEnterResult = HandleEnterEffect(),
  io:format(user, "Enter result: ~p~n", [HandleEnterResult]),
  case HandleEnterResult of
    {outerStateEnterOk, NewData} -> {keep_state, NewData};
    {outerStateEnterOkWithActions, NewData, Actions} -> {keep_state, NewData, state_enter_actions(Actions)};
    {outerStateEnterKeepData} -> keep_state_and_data;
    {outerStateEnterKeepDataWithActions, Actions} -> {keep_state_and_data, state_enter_actions(Actions)}
  end;

handle_event({call, From}, Fn, State, Data) ->
  io:format(user, "Got call event ~p:~p in state ~p (~p)~n", [From, Fn, State, Data]),
  Effect = Fn(From, Data),
  Result = Effect(),
  io:format(user, "Call result: ~p~n", [Result]),
  event_result_from_ps(Result);

handle_event(cast, Fn, State, Data) ->
  io:format(user, "Got cast event ~p in state ~p (~p)~n", [Fn, State, Data]),
  Effect = Fn(Data),
  Result = Effect(),
  io:format(user, "Cast result: ~p~n", [Result]),
  event_result_from_ps(Result);

handle_event(Event, EventContent, State, #{ handleEvent := HandleEvent } = Data) ->
  EventPS = event_to_ps(Event, EventContent),
  io:format(user, "Got event ~p in state ~p (~p)~n", [EventPS, State, Data]),
  Effect = HandleEvent(EventPS, Data),
  Result = Effect(),
  io:format(user, "Event result: ~p~n", [Result]),
  event_result_from_ps(Result).


%%% ----------------------------------------------------------------------------
%%% FFI Support
%%% ----------------------------------------------------------------------------
init_actions(Actions) ->
  event_actions(Actions).

state_enter_actions(Actions) ->
  lists:map(fun common_action/1, Actions).

event_actions(Actions) ->
  lists:map(fun event_action/1, Actions).

event_action({commonAction, CommonAction}) -> common_action(CommonAction);
event_action({postpone}) -> postpone;
event_action({nextEvent, _Event}) -> throw(not_implemented).

common_action({hibernate}) -> hibernate;
common_action({timeoutAction, TimeoutAction}) -> timeout_action(TimeoutAction);
common_action({namedTimeoutAction, NamedTimeoutAction}) -> named_timeout_action(NamedTimeoutAction);
common_action({replyAction, Reply}) -> Reply.

timeout_action({setTimeout, {'at', T, Content}}) -> {timeout, T, Content, [{abs, true}]};
timeout_action({setTimeout, {'after', T, Content}}) -> {timeout, T, Content};
timeout_action({setTimeout, cancel}) -> {timeout, cancel};

timeout_action({setStateTimeout, {'at', T, Content}}) -> {state_timeout, T, Content, [{abs, true}]};
timeout_action({setStateTimeout, {'after', T, Content}}) -> {state_timeout, T, Content};
timeout_action({setStateTimeout, cancel}) -> {state_timeout, cancel};

timeout_action({updateTimeout, Content}) -> {timeout, update, Content};
timeout_action({updateTimeout, cancel}) -> {timeout, cancel};

timeout_action({updateStateTimeout, Content}) -> {state_timeout, update, Content};
timeout_action({updateStateTimeout, cancel}) -> {state_timeout, cancel}.

named_timeout_action(_Action) -> throw(not_implemented).

event_to_ps(info, Info) -> {eventInfo, Info};
event_to_ps(internal, Internal) -> {eventInternal, Internal};
event_to_ps(timeout, Content) ->  {eventTimeout, Content};
event_to_ps({timeout, Name}, Content) -> {eventNamedTimeout, Name, Content};
event_to_ps(state_timeout, Content) -> {eventStateTimeout, Content}.

event_result_from_ps(Result) ->
  case Result of
    {outerEventKeepStateAndData} -> keep_state_and_data;
    {outerEventKeepStateAndDataWithActions, Actions} -> {keep_state_and_data, event_actions(Actions)};
    {outerEventKeepState, NewData} -> {keep_state, NewData};
    {outerEventKeepStateWithActions, NewData, Actions} -> {keep_state, NewData, event_actions(Actions)};
    {outerEventNextState, NewState, NewData} -> {next_state, NewState, NewData};
    {outerEventNextStateWithActions, NewState, NewData, Actions} -> {next_state, NewState, NewData, event_actions(Actions)}
  end.

statem_ref_from_ps({byName, PsCallName})               -> instance_name_from_ps(PsCallName);
statem_ref_from_ps({byPid, Pid})                       -> Pid.
