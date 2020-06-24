-module(pinto_gen@foreign).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

% FFI
-export([startLinkImpl/3,
         stopImpl/1,
         callImpl/2,
         doCallImpl/2,
         castImpl/2,
         doCastImpl/2
        ]).

% Pinto supervision entry point
-export([start_from_spec/1,
         start_from_spec/2
        ]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% Pinto specific APIs
-export([emitterImpl/1,
         whereIsImpl/3,
         logWarning/2
        ]).

-record(state_impl,
        {
         state :: term(),
         handle_info :: fun(),
         terminate_handler :: undefined | fun(),
         trap_exit :: undefined | fun()
         }).

doCallImpl(Name, Fn) -> fun() ->
                            gen_server:call(Name, { wrapped_effectful_call, Fn })
                        end.

callImpl(Name, Fn) -> fun() ->
                          gen_server:call(Name, { wrapped_pure_call, Fn })
                      end.

doCastImpl(Name, Fn) -> fun() ->
                            gen_server:cast(Name, { wrapped_effectful_cast, Fn })
                        end.

castImpl(Name, Fn) -> fun() ->
                          gen_server:cast(Name, { wrapped_pure_cast, Fn })
                      end.

stopImpl(Name) -> fun() ->
                      gen_server:stop(Name)
                  end.


logWarning(Message, Metadata) ->
  fun() ->
      ?LOG_WARNING(Message,[], [Metadata])
  end.

startLinkImpl(Name, Effect, Opts) ->
  fun() ->
      gen_server:start_link(Name, ?MODULE, [Effect, Opts], [])
  end.

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

start_from_spec(_Spec = #{ startFn := Fn }, Args) ->
  (Fn(Args))().

%% An emitter is a function that given a message returns an Effect Unit
%% And this function returns a function of (Msg -> Effect Unit)
%% So given a message we need to return an Effect Unit which is.. another function
%% and the function to get the emitter in the first place is effectful (where_is_name)
%% so that's another effect, anyway - think about this v carefully before you edit
emitterImpl(Name) ->
  fun() ->
    Pid  = where_is_name(Name),
    fun(Msg) ->
        fun() ->
          Pid ! Msg
        end
    end
  end.

whereIsImpl(Name, Just, Nothing) ->
  fun() ->
    case where_is_name(Name) of
      undefined -> Nothing;
      Pid -> Just(Pid)
    end
  end.

init([Effect, #{ handleInfo := HandleInfo
               , terminate := MaybeTerminate
               , trapExit  := MaybeTrapExit
   }]) ->
  {ok, #state_impl { state = Effect()
                   , handle_info = HandleInfo
                   , terminate_handler = case MaybeTerminate of
                                           {nothing} -> undefined;
                                           {just, Terminate} -> Terminate
                                         end
                   , trap_exit = case MaybeTrapExit of
                                   {nothing} -> undefined;
                                   {just, TrapExit} ->
                                     process_flag(trap_exit, true),
                                     TrapExit
                                 end
                   }}.

handle_call({wrapped_effectful_call, Fn}, _From, StateImpl = #state_impl { state = State } ) ->
  dispatch_call_response((Fn(State))(), StateImpl);

handle_call({wrapped_pure_call, Fn}, _From, StateImpl = #state_impl { state = State }) ->
  dispatch_call_response(Fn(State), StateImpl).

handle_cast({wrapped_effectful_cast, Fn}, StateImpl = #state_impl { state = State }) ->
  dispatch_cast_response((Fn(State))(), StateImpl);

handle_cast({wrapped_pure_cast, Fn}, StateImpl = #state_impl { state = State }) ->
  dispatch_cast_response(Fn(State), StateImpl).

handle_info({'EXIT', FromPid, Reason}, StateImpl = #state_impl { trap_exit = TrapExitMsg, handle_info = HandleInfo, state = State }) ->
  dispatch_cast_response(((HandleInfo(TrapExitMsg({exit, FromPid,  Reason})))(State))(), StateImpl);

handle_info(Msg, StateImpl = #state_impl { state = State, handle_info = HandleInfo}) ->
  dispatch_cast_response(((HandleInfo(Msg))(State))(), StateImpl).

terminate(_Reason, _StateImpl = #state_impl { terminate_handler = undefined} ) ->
  ok;

terminate(Reason, _StateImpl = #state_impl { terminate_handler = TerminateHandler
                                           , state = State } ) ->
  ((TerminateHandler(map_shutdown_reason_to_purs(Reason)))(State))().

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

dispatch_call_response(Response, StateImpl) ->
  case Response of
    { callReply, Result, NewState } -> {reply, Result, StateImpl#state_impl { state = NewState}};
    { callReplyHibernate, Result, NewState } -> {reply, Result, StateImpl#state_impl { state = NewState }, hibernate};
    { callStop, Result, NewState } -> {stop, normal, Result, StateImpl#state_impl { state = NewState }}
  end.

dispatch_cast_response(Response, StateImpl) ->
  case Response of
    { castNoReply, NewState } -> {noreply, StateImpl#state_impl { state = NewState}};
    { castNoReplyHibernate, NewState } -> {noreply, StateImpl#state_impl { state = NewState }, hibernate};
    { castStop, NewState } -> {stop, normal, StateImpl#state_impl { state = NewState }};
    { castStopReason, Reason, NewState} -> {stop, map_shutdown_reason_to_erl(Reason), StateImpl#state_impl { state = NewState }}
  end.

map_shutdown_reason_to_purs(normal) ->
  {normal};
map_shutdown_reason_to_purs(shutdown) ->
  {shutdown};
map_shutdown_reason_to_purs({shutdown, Term}) ->
  {shutdownWithCustom, Term};
map_shutdown_reason_to_purs(Term) ->
  {custom, Term}.

map_shutdown_reason_to_erl({normal}) ->
  normal;
map_shutdown_reason_to_erl({shutdown}) ->
  shutdown;
map_shutdown_reason_to_erl({shutdownWithCustom, Term}) ->
  {shutdown, Term};
map_shutdown_reason_to_erl({custom, Term}) ->
  Term.

where_is_name(GenName)  ->
  case GenName of
    P when is_pid(P) ->
      P;
    {via, Module, Name} ->
      Module:whereis_name(Name);
    {global, Name} ->
      global:whereis_name(Name);
    Name when is_atom(Name) ->
      whereis(Name)
  end.

