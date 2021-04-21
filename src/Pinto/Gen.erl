-module(pinto_gen@foreign).

-include_lib("kernel/include/logger.hrl").

-export([startLinkImpl/3
       , unpackArgsImpl/1
       , stopImpl/1
       , doCallImpl/2
       , doCastImpl/2
       , callReplyImpl/2
       , callNoReplyImpl/1
       , callReplyHibernateImpl/2
       , callStopImpl/3
       , castNoReplyImpl/1
       , castNoReplyHibernateImpl/1
       , castStopImpl/2
       , enableTrapExitImpl/0
       , mapInfoMessageImpl/3
       , readTerminateReasonImpl/5
       , start_from_spec/1
       , start_from_spec/2
       , mapInfoMessageImpl/3
       , doReplyImpl/2
     ]).


%% Pinto specific APIs
-export([whereIsImpl/3,
         logWarning/2,
         selfImpl/0
        ]).

callReplyImpl(Resp, NewState) -> { reply, Resp, NewState }.
callNoReplyImpl(NewState) -> { noreply, NewState }.
callReplyHibernateImpl(Resp, NewState) -> { reply, Resp, NewState, hibernate }.
callStopImpl(Reason, Resp, NewState) -> {stop, Reason, Resp, NewState }.

castNoReplyImpl(NewState) -> {noreply, NewState }.
castNoReplyHibernateImpl(NewState) -> {noreply, NewState, hibernate }.
castStopImpl(Reason, NewState) -> { stop, Reason, NewState }.

doCallImpl(Name, Fn) -> fun() -> gen_server:call(Name, Fn) end.
doCastImpl(Name, Fn) -> fun() -> gen_server:cast(Name, Fn) end.
stopImpl(Name) -> fun() -> gen_server:stop(Name) end.

mapInfoMessageImpl({just, Mapper}, Constructor, { 'EXIT', Pid,  Reason }) ->
  Mapper((Constructor(Pid))(Reason));
mapInfoMessageImpl(_, _, Other) ->  Other. %% Waves Hands

doReplyImpl(Reply, From) ->
  fun() ->
      gen_server:reply(From, Reply),
      unit
  end.

selfImpl() ->
  fun()  ->
      self()
  end.


logWarning(Message, Info) ->
  fun() ->
      ?LOG_WARNING(Message, Info)
  end.

startLinkImpl(Name, Init, Opts) ->
  fun() ->
      gen_server:start_link(Name, 'pinto_gen@ps', [ #{ init => Init, opts => Opts  }], [])
  end.

unpackArgsImpl([Args]) -> Args. %% shrug

start_from_spec(_Spec = #{ startFn := Fn, startArgs := Args }) ->
  (Fn(Args))().

start_from_spec(_Spec = #{ startFn := Fn }, Args) ->
  (Fn(Args))().

whereIsImpl(Name, Just, Nothing) ->
  fun() ->
    case where_is_name(Name) of
      undefined -> Nothing;
      Pid -> Just(Pid)
    end
  end.

enableTrapExitImpl() ->
  fun() ->
      erlang:process_flag(trap_exit, true)
  end.

readTerminateReasonImpl(F, Normal, Shutdown, ShutdownWithCustom, Custom) ->
  case F of
    normal -> Normal;
    shutdown -> Shutdown;
    {shutdown, Custom} -> ShutdownWithCustom(Custom);
    Other -> Custom(Other)
  end.

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
