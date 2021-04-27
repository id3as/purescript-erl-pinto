-module(pinto_genServer@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ selfFFI/0
        , startLinkFFI/2
        , callFFI/2
        , castFFI/2
        , replyTo/2
        , stopFFI/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , handle_continue/2
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        ]).

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
startLinkFFI(MaybeName, InitEffect) ->
  fun() ->
      Result =
        case MaybeName of
          {nothing} ->
            gen_server:start_link(?MODULE, [InitEffect], []);
          {just, Name} ->
            gen_server:start_link(Name, ?MODULE, [InitEffect], [])
        end,

      start_link_result_to_ps(Result)
  end.

castFFI(ServerRef, CastFn) ->
  fun() ->
      gen_server:cast(ServerRef, {do_cast, CastFn}),
      unit
  end.

callFFI(ServerRef, CallFn) ->
  fun() ->
      gen_server:call(ServerRef, {do_call, CallFn})
  end.

replyTo(From, Reply) ->
  fun() ->
      gen_server:reply(From, Reply),
      unit
  end.

selfFFI() ->
  fun() ->
      self()
  end.

stopFFI(ServerRef) ->
  fun() ->
      gen_server:stop(ServerRef),
      unit
  end.

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------
init([InitEffect]) ->
  InitResult = InitEffect(),

  case InitResult of
    {initStop, Error}                  -> {stop, Error};
    {initIgnore}                       -> ignore;

    {initOk, State}                   -> {ok, State};
    {initOkTimeout, State, Timeout}   -> {ok, State, Timeout};
    {initOkContinue, State, Continue} -> {ok, State, {continue, Continue}};
    {initOkHibernate, State}          -> {ok, State, hibernate}
  end.


handle_call({do_call, CallFn}, From, State) ->
  CallEffect = CallFn(From, State),
  CallResult = CallEffect(),

  case CallResult of
    {callResult, {just, Reply}, {nothing}, NewState}                       -> {reply, Reply, NewState};
    {callResult, {just, Reply}, {just, {timeout, Timeout}}, NewState}      -> {reply, Reply, NewState, Timeout};
    {callResult, {just, Reply}, {just, {hibernate}}, NewState}             -> {reply, Reply, NewState, hibernate};
    {callResult, {just, Reply}, {just, {continue, Continue}}, NewState}    -> {reply, Reply, NewState, {continue, Continue}};
    {callResult, {just, Reply}, {just, {stopNormal}}, NewState}            -> {stop, normal, Reply, NewState};
    {callResult, {just, Reply}, {just, {stopOther, StopReason}}, NewState} -> {stop, StopReason, Reply, NewState};

    {callResult, {nothing}, {nothing}, NewState}                           -> {noreply, NewState};
    {callResult, {nothing}, {just, {timeout, Timeout}}, NewState}          -> {noreply, NewState, Timeout};
    {callResult, {nothing}, {just, {hibernate}}, NewState}                 -> {noreply, NewState, hibernate};
    {callResult, {nothing}, {just, {continue, Continue}}, NewState}        -> {noreply, NewState, {continue, Continue}};
    {callResult, {noreply}, {just, {stopNormal}}, NewState}                -> {stop, normal, NewState};
    {callResult, {noreply}, {just, {stopOther, StopReason}}, NewState}     -> {stop, StopReason, NewState}
  end.

handle_cast({do_cast, CastFn}, State) ->
  ResultEffect = CastFn(State),
  Result = ResultEffect(),
  return_result_to_ps(Result).

handle_info(Msg, #{ context := #{ handleInfo := {just, WrappedHandler } } } = State) ->
  ResultEffect = WrappedHandler(Msg, State),
  Result = ResultEffect(),
  return_result_to_ps(Result).

handle_continue(Msg, #{ context := #{ handleContinue := {just, WrappedHandler } } } = State) ->
  ResultEffect = WrappedHandler(Msg, State),
  Result = ResultEffect(),
  return_result_to_ps(Result).

return_result_to_ps(ReturnResult) ->
  case ReturnResult of
    {returnResult, {nothing}, NewState}                       -> {noreply, NewState};
    {returnResult, {just, {timeout, Timeout}}, NewState}      -> {noreply, NewState, Timeout};
    {returnResult, {just, {hibernate}}, NewState}             -> {noreply, NewState, hibernate};
    {returnResult, {just, {continue, Continue}}, NewState}    -> {noreply, NewState, {continue, Continue}};
    {returnResult, {just, {stopNormal}}, NewState}            -> {stop, normal, NewState};
    {returnResult, {just, {stopOther, StopReason}}, NewState} -> {stop, StopReason, NewState}
  end.
