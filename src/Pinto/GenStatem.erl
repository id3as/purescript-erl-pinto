-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

%% FFI Exports
-export([ startLinkFFI/2
        , selfFFI/0
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
mkReply(_) ->
  fun() ->
      ok
  end.

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startLinkFFI(MaybeName, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            gen_statem:start_link(?MODULE, InitEffect, []);
          {just, NamePS} ->
            Name = registry_name_from_ps(NamePS),
            gen_statem:start_link(Name, ?MODULE, InitEffect, [])
        end,

      start_link_result_to_ps(Result)
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
    {outerInitOkWithActions, State, Data, Actions} -> {ok, State, Data, Actions};
    {outerInitStop, Error} -> {stop, Error};
    {outerInitIgnore} -> ignore
  end.

handle_event(enter, OldState, NewState, #{ handleEnter := HandleEnter } = Data) ->
  io:format(user, "Got state enter ~p -> ~p (~p)~n", [OldState, NewState, Data]),
  HandleEnterEffect = HandleEnter(OldState, NewState, Data),
  HandleEnterResult = HandleEnterEffect(),
  io:format(user, "Enter result: ~p~n", [HandleEnterResult]),
  case HandleEnterResult of
    {outerStateEnterOk, Data} -> {keep_state, Data};
    {outerStateEnterOkWithActions, Data, Actions} -> {keep_state, Data, Actions};
    {outerStateEnterKeepData} -> keep_state_and_data;
    {outerStateEnterKeepDataWithActions, Actions} -> {keep_state_and_data, Actions}
  end;

handle_event(Event, EventContent, State, #{} = Data) ->
  io:format(user, "Got event ~p:~p in state ~p (~p)~n", [Event, EventContent, State, Data]),
  keep_state_and_data.
