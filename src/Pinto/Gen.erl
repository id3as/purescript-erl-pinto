-module(pinto_gen@foreign).

-behaviour(gen_server).

% FFI
-export([
         startLinkImpl/2,
         callImpl/2,
         doCallImpl/2
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

doCallImpl(Name, Fn) -> fun() ->
                            gen_server:call(Name, { wrapped_effectful_call, Fn })
                        end.

callImpl(Name, Fn) -> fun() ->
                            gen_server:call(Name, { wrapped_pure_call, Fn })
                        end.

startLinkImpl(Name, Effect) ->
  fun() ->
      gen_server:start_link({local, Name}, ?MODULE, [Effect], [])
  end.

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

start_from_spec(_Spec = #{ startFn := Fn }, Args) ->
  (Fn(Args))().


init([Effect]) ->
  {ok, Effect()}.


handle_call({wrapped_effectful_call, Fn}, _From, State) ->
  case (Fn(State))() of
    { reply, Result, NewState } -> {reply, Result, NewState};
    { stop, Result, NewState } -> {stop, normal, Result, NewState}
  end;

handle_call({wrapped_pure_call, Fn}, _From, State) ->
  case Fn(State) of
    { reply, Result, NewState } -> {reply, Result, NewState};
    { stop, Result, NewState } -> {stop, normal, Result, NewState}
  end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({routed_message, Fn}, State) ->
  NewState = (Fn(State))(),
  {noreply, NewState};

handle_info({routed_message, Msg, Fn}, State) ->
  NewState = ((Fn(Msg))(State))(),
  {noreply, NewState}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
