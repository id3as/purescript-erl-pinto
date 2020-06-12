-module(pinto_gen@foreign).

-behaviour(gen_server).

% FFI
-export([
         startLinkImpl/3,
         stopImpl/1,
         callImpl/2,
         doCallImpl/2,
         castImpl/2,
         doCastImpl/2
        ]).

% Pinto supervision entry point
-export([
         start_from_spec/1,
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
-export([
         registerExternalMappingImpl/2,
         registerTerminateImpl/2,
         monitorImpl/3
        ]).

-record(state_impl,
        {
         state :: term(),
         handle_info :: fun(),
         mappings :: list(fun()),
         monitors = #{} :: maps:map(pid(), {reference(), fun()}),
         terminate_handler :: undefined | fun()
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

startLinkImpl(Name, Effect, HandleInfo) ->
  fun() ->
      gen_server:start_link(Name, ?MODULE, [Effect, HandleInfo], [])
  end.

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

start_from_spec(_Spec = #{ startFn := Fn }, Args) ->
  (Fn(Args))().

%% This is wrong, ideally we'd just be using the current context
%% of the call to work out where we were and just manipulating the state
%% I think we can do this if we use a state monad...
registerExternalMappingImpl(Name, Mapper) ->
  fun() ->
    gen_server:cast(Name, { register_mapping, Mapper })
  end.

registerTerminateImpl(Name, TerminateHandler) ->
  fun() ->
    gen_server:cast(Name, { register_terminate, TerminateHandler })
  end.

%% Similar approach to registerExternalMappingImpl - given a state monad, we could probably
%% make this better and return a monitor ref to enable demonitor calls
monitorImpl(Name, ToMonitor, Mapper) ->
  fun() ->
      gen_server:cast(Name, { monitor, ToMonitor, Mapper })
  end.

init([Effect, HandleInfo]) ->
  {ok, #state_impl { state = Effect()
                   , handle_info = HandleInfo
                   , mappings = []
                   }}.

handle_call({wrapped_effectful_call, Fn}, _From, StateImpl = #state_impl { state = State } ) ->
  dispatch_call_response((Fn(State))(), StateImpl);

handle_call({wrapped_pure_call, Fn}, _From, StateImpl = #state_impl { state = State }) ->
  dispatch_call_response(Fn(State), StateImpl).

handle_cast({wrapped_effectful_cast, Fn}, StateImpl = #state_impl { state = State }) ->
  dispatch_cast_response((Fn(State))(), StateImpl);

handle_cast({wrapped_pure_cast, Fn}, StateImpl = #state_impl { state = State }) ->
  dispatch_cast_response(Fn(State), StateImpl);

handle_cast({register_mapping, Mapping}, StateImpl = #state_impl { mappings = Mappings }) ->
  { noreply, StateImpl#state_impl { mappings = [ Mapping | Mappings ] }};

handle_cast({register_terminate, TerminateHandler}, StateImpl = #state_impl { }) ->
  { noreply, StateImpl#state_impl { terminate_handler = TerminateHandler }};

handle_cast({ monitor, ToMonitor, Mapper }, StateImpl = #state_impl { monitors = Monitors }) ->

  Pid = case ToMonitor of
          P when is_pid(P) ->
            P;
          {via, Module, Name} ->
            Module:whereis_name(Name);
          {global, Name} ->
            global:whereis_name(Name);
          Name when is_atom(Name) ->
            whereis(Name)
        end,

  case Pid of
    undefined ->
      %% DOWN
      Ref = make_ref(),
      Monitors2 = maps:put(Ref, {ToMonitor, Mapper}, Monitors),

      handle_info({'DOWN', Ref, process, undefined, noproc}, StateImpl#state_impl{ monitors = Monitors2 });
    _ ->
      MRef = erlang:monitor(process, Pid),
      Monitors2 = maps:put(MRef, {ToMonitor, Mapper}, Monitors),

      {noreply, StateImpl#state_impl{ monitors = Monitors2 }}
  end.

handle_info({'DOWN', MRef, _Type, _Object, Info}, StateImpl = #state_impl { state = State, handle_info = HandleInfo, monitors = Monitors }) ->

  case maps:find(MRef, Monitors) of
    error ->
      {noreply, StateImpl};

    {ok, {_ToMonitor, Fun}} ->
      MappedMsg = Fun(Info),
      dispatch_cast_response(((HandleInfo(MappedMsg))(State))(), StateImpl)
  end;

handle_info(Msg, StateImpl = #state_impl { state = State, handle_info = HandleInfo, mappings = Mappings }) ->
  MappedMsg = try_map(Msg, Mappings),
  dispatch_cast_response(((HandleInfo(MappedMsg))(State))(), StateImpl).

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

try_map(Msg, []) -> Msg;
try_map(Msg, [ Head | Tail ]) ->
  case Head(Msg) of
    {just, Mapped} -> Mapped;
    {nothing} -> try_map(Msg, Tail)
  end.
