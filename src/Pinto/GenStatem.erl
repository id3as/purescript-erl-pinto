-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

%% FFI Exports
-export([ startLinkFFI/2
        , selfFFI/0
        , callFFI/2
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
        , instance_ref_from_ps/1
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

callFFI(InstanceRef, CallFn) ->
  fun() ->
      io:format(user, "Issuing call to ~p~n", [instance_ref_from_ps(InstanceRef)]),
      gen_server:call(instance_ref_from_ps(InstanceRef), CallFn)
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

handle_event({call, From}, CallFn, State, Data) ->
  io:format(user, "Got call event ~p:~p in state ~p (~p)~n", [From, CallFn, State, Data]),
  CallEffect = CallFn(From, Data),
  CallResult = CallEffect(),
  io:format(user, "Call result: ~p~n", [CallResult]),
  case CallResult of
    {outerCallKeepStateAndData} -> keep_state_and_data;
    {outerCallKeepStateAndDataWithActions, Actions} -> {keep_state_and_data, event_actions(Actions)};
    {outerCallKeepState, NewData} -> {keep_state, NewData};
    {outerCallKeepStateWithActions, NewData, Actions} -> {keep_state, NewData, event_actions(Actions)};
    {outerCallNextState, NewState, NewData} -> {next_state, NewState, NewData};
    {outerCallNextStateWithActions, NewState, NewData, Actions} -> {next_state, NewState, NewData, event_actions(Actions)}
  end;

handle_event(Event, EventContent, State, Data) ->
  io:format(user, "Got event ~p:~p in state ~p (~p)~n", [Event, EventContent, State, Data]),
  keep_state_and_data.


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

timeout_action({setTimeout, {'at', T, Msg}}) -> {timeout, T, Msg, [{abs, true}]};
timeout_action({setTimeout, {'after', T, Msg}}) -> {timeout, T, Msg};
timeout_action({setTimeout, cancel}) -> {timeout, cancel};

timeout_action({setStateTimeout, {'at', T, Msg}}) -> {state_timeout, T, Msg, [{abs, true}]};
timeout_action({setStateTimeout, {'after', T, Msg}}) -> {state_timeout, T, Msg};
timeout_action({setStateTimeout, cancel}) -> {state_timeout, cancel};

timeout_action({updateTimeout, Msg}) -> {timeout, update, Msg};
timeout_action({updateTimeout, cancel}) -> {timeout, cancel};

timeout_action({updateStateTimeout, Msg}) -> {state_timeout, update, Msg};
timeout_action({updateStateTimeout, cancel}) -> {state_timeout, cancel}.

named_timeout_action(_Action) -> throw(not_implemented).
