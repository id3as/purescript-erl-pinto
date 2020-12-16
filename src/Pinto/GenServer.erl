-module(pinto_genServer@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ selfFFI/0
        , startLinkFFI/2
        , callFFI/2
        ]).

-export([ init/1
        , handle_info/2
        , handle_call/3
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        , registry_name_from_ps/1
        , instance_ref_from_ps/1
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
          {just, NamePS} ->
            Name = registry_name_from_ps(NamePS),
            gen_server:start_link(Name, ?MODULE, [InitEffect], [])
        end,

      start_link_result_to_ps(Result)
  end.


callFFI(ServerRef, CallFn) ->
  fun() ->
      gen_server:call(instance_ref_from_ps(ServerRef), {do_call, CallFn})
  end.

selfFFI() ->
  fun() ->
      self()
  end.

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------
init([InitEffect]) ->

  InitResult = InitEffect(),

  case InitResult of
    {left, {initStop, Error}}                  -> {stop, Error};
    {left, {initIgnore}}                       -> ignore;

    {right, {initOk, State}}                   -> {ok, State};
    {right, {initOkTimeout, State, Timeout}}   -> {ok, State, Timeout};
    {right, {initOkContinue, State, Continue}} -> {ok, State, {continue, Continue}};
    {right, {initOkHibernate, State}}          -> {ok, State, hibernate}
  end.


handle_call({do_call, CallFn}, _From, State) ->
  CallEffect = CallFn(State),
  CallResult = CallEffect(),

  case CallResult of
    {callReply, Reply, NewState}                     -> {reply, Reply, NewState};
    {callReplyWithTimeout, Reply, NewState, Timeout} -> {reply, Reply, NewState, Timeout};
    {callReplyHibernate, Reply, NewState}            -> {reply, Reply, NewState, hibernate};
    {callReplyContinue, Reply, NewState, Continue}   -> {reply, Reply, NewState, {continue, Continue}};

    {callNoReply, NewState}                          -> {noreply, NewState};
    {callNoReplyWithTimeout, NewState, Timeout}      -> {noreply, NewState, Timeout};
    {callNoReplyHibernate, NewState}                 -> {noreply, NewState, hibernate};
    {callNoReplyContinue, NewState, Continue}        -> {noreply, NewState, {continue, Continue}};

    {callStopReply, Reason, Reply, NewState}         -> {stop, Reason, Reply, NewState};
    {callStopNoReply, Reason, NewState}              -> {stop, Reason, NewState}
  end.


handle_info(Msg, #{ context := #{ handleInfo := {just, WrappedHandleInfo } } } = OuterState) ->

  InfoResultEffect = WrappedHandleInfo(Msg, OuterState),

  case InfoResultEffect() of
    {noReply, NewOuterState}                   -> {noreply, NewOuterState};
    {noReplyTimeout, NewOuterState, Timeout}   -> {noreply, NewOuterState, Timeout};
    {noReplyHibernate, NewOuterState}          -> {noreply, NewOuterState, hibernate};
    {noReplyContinue, NewOuterState, Continue} -> {noreply, NewOuterState, {continue, Continue}};
    {stop, Reason, NewOuterState}              -> {stop, Reason, NewOuterState}
  end.
