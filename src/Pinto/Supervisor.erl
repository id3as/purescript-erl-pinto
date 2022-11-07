-module(pinto_supervisor@foreign).

%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
-export([ specFFI/1
        , startLink/2
        , stopFFI/2
        , startChildFFI/2
        , terminateChildFFI/2
        , deleteChildFFI/2
        ]).


%%------------------------------------------------------------------------------
%% Runtime supervisor stubs
%%------------------------------------------------------------------------------
-export([ start_proxy/1
        , init/1
        ]).

%% used by dynamic sup
-export([ restart_from_ps/1,
          shutdown_from_ps/1,
          type_from_ps/1
        ]).

-purs_ignore_exports([start_proxy/1, init/1, restart_from_ps/1, shutdown_from_ps/1, type_from_ps/1]). 

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        , start_link_result_from_ps/1
        ]).

init(EffectSupervisorSpec) ->
  #{ flags := Flags
   , childSpecs := ChildSpecs
   } = EffectSupervisorSpec(),

  {ok, {flags_from_ps(Flags), ChildSpecs}}.


%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
specFFI(#{ id := ChildId
         , start := StartFn
         , restartStrategy := RestartStrategy
         , shutdownStrategy := ChildShutdownTimeoutStrategy
         , childType := ChildType
         }) ->
  #{ id => ChildId
   , start => {?MODULE, start_proxy, [StartFn]}
   , restart => restart_from_ps(RestartStrategy)
   , shutdown => shutdown_from_ps(ChildShutdownTimeoutStrategy)
   , type => type_from_ps(ChildType)
   }.


startLink(Name, EffectSupervisorSpec) ->
  fun() ->
      startLinkPure(Name, EffectSupervisorSpec)
  end.

startLinkPure({nothing}, EffectSupervisorSpec) ->
  Result = supervisor:start_link(?MODULE, EffectSupervisorSpec),
  start_link_result_to_ps(Result);
startLinkPure({just, Name}, EffectSupervisorSpec) ->
  Result = supervisor:start_link(Name, ?MODULE, EffectSupervisorSpec),
  start_link_result_to_ps(Result).

stopFFI(Timeout, RefOrPid) ->
  fun() ->
    gen:stop(RefOrPid, normal, case Timeout of {stopInfinity} -> infinity; {stopTimeout, Ms} -> trunc(Ms) end)
  end.

startChildFFI(Ref, ChildSpec) ->
  fun() ->
      Result = supervisor:start_child(Ref, ChildSpec),
      start_child_result_to_ps(Result)
  end.

terminateChildFFI(Ref, Id) ->
  fun() ->
      Result = supervisor:terminate_child(Ref, Id),
      terminate_child_result_to_ps(Result)
  end.

deleteChildFFI(Ref, Id) ->
  fun() ->
      Result = supervisor:delete_child(Ref, Id),
      delete_child_result_to_ps(Result)
  end.

%%------------------------------------------------------------------------------
%% erlang -> ps conversion helpers
%%------------------------------------------------------------------------------
start_child_result_to_ps({ok, undefined})                 -> {childStartReturnedIgnore};
start_child_result_to_ps({ok, {Pid, Info}})               -> {childStarted, #{pid => Pid, info => {just, Info}}};
start_child_result_to_ps({ok, Pid})                       -> {childStarted, #{pid => Pid, info => {nothing}}};
start_child_result_to_ps({error, already_present})        -> {childAlreadyPresent};
start_child_result_to_ps({error, {already_started, Pid}}) -> {childAlreadyStarted, Pid};
start_child_result_to_ps({error, Other})                  -> {childFailed, Other}.

%% These are deliberately not exhaustive, as this code path shouldn't be hit with simple_one_for_one
delete_child_result_to_ps(ok)                 -> {childDeleted};
delete_child_result_to_ps({error, running})                 -> {childRunning};
delete_child_result_to_ps({error, not_found})                 -> {childNotFoundToDelete}.

terminate_child_result_to_ps(ok)                 -> {childTerminated};
terminate_child_result_to_ps({error, not_found})                 -> {childNotFoundToTerminate}.

%%------------------------------------------------------------------------------
%% ps -> erlang conversion helpers
%%------------------------------------------------------------------------------
flags_from_ps( #{ strategy := Strategy
                , intensity := Intensity
                , period := Period
                }) ->
  #{ strategy => strategy_from_ps(Strategy)
   , intensity => Intensity
   , period => round(Period)
   }.

%% Note: These could be done in Purerl too and next time we're down thie way, probably will be

strategy_from_ps({oneForAll}) -> one_for_all;
strategy_from_ps({oneForOne}) -> one_for_one;
strategy_from_ps({restForOne}) -> rest_for_one.

restart_from_ps({restartTransient}) -> transient;
restart_from_ps({restartPermanent}) -> permanent;
restart_from_ps({restartTemporary}) -> temporary.

shutdown_from_ps({shutdownBrutal}) -> brutal;
shutdown_from_ps({shutdownInfinity}) -> infinity;
shutdown_from_ps({shutdownTimeout, Ms}) ->  round(Ms).

type_from_ps({supervisor}) -> supervisor;
type_from_ps({worker}) -> worker.

start_proxy(StartEffect) ->
  StartResult = StartEffect(),
  start_link_result_from_ps(StartResult).
