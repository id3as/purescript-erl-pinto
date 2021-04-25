-module(pinto_sup@foreign).

%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
-export([ mkErlChildSpecFFI/1
        , startLink/2
        , stop/1
        , startChildFFI/2
        ]).


%%------------------------------------------------------------------------------
%% Runtime supervisor stubs
%%------------------------------------------------------------------------------
-export([ start_proxy/1
        , init/1
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        , start_link_result_from_ps/1

        , registry_name_from_ps/1
        , instance_name_from_ps/1
        ]).

init(EffectSupervisorSpec) ->
  #{ flags := Flags
   , childSpecs := ChildSpecs
   } = EffectSupervisorSpec(),

  {ok, {flags_from_ps(Flags), ChildSpecs}}.


%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
mkErlChildSpecFFI(#{ id := ChildId
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
startLinkPure({just, RegistryName}, EffectSupervisorSpec) ->
  Result = supervisor:start_link(registry_name_from_ps(RegistryName), ?MODULE, EffectSupervisorSpec),
  start_link_result_to_ps(Result).

stop(RegistryName) ->
  fun() ->
    sys:terminate(registry_name_from_ps(RegistryName), shutdown)
  end.

startChildFFI(SupRef, ChildSpec) ->
  fun() ->
      startChildPure(SupRef, mkErlChildSpecFFI(ChildSpec))
  end.

startChildPure({byPid, Pid}, ChildSpec) ->
  Result = supervisor:start_child(Pid, ChildSpec),
  start_child_result_to_ps(Result);
startChildPure({byName, Name}, ChildSpec) ->
  Result = supervisor:start_child(instance_name_from_ps(Name), ChildSpec),
  start_child_result_to_ps(Result).




%%------------------------------------------------------------------------------
%% erlang -> ps conversion helpers
%%------------------------------------------------------------------------------
start_child_result_to_ps({ok, undefined})                 -> {left, {childStartReturnedIgnore}};
start_child_result_to_ps({ok, {Pid, Info}})               -> {right, #{pid => Pid, info => {just, Info}}};
start_child_result_to_ps({ok, Pid})                       -> {right, #{pid => Pid, info => {nothing}}};
start_child_result_to_ps({error, already_present})        -> {left, {childAlreadyPresent}};
start_child_result_to_ps({error, {already_started, Pid}}) -> {left, {childAlreadyStarted, Pid}};
start_child_result_to_ps({error, Other})                  -> {left, {childFailed, Other}}.

%%------------------------------------------------------------------------------
%% ps -> erlang conversion helpers
%%------------------------------------------------------------------------------
flags_from_ps( #{ strategy := Strategy
                , intensity := Intensity
                , period := Period
                }) ->
  #{ strategy => strategy_from_ps(Strategy)
   , intensity => Intensity
   , period => Period
   }.


strategy_from_ps({oneForAll}) -> one_for_all;
strategy_from_ps({oneForOne}) -> one_for_one;
strategy_from_ps({restForOne}) -> rest_for_one.

restart_from_ps({restartNever}) -> transient;
restart_from_ps({restartAlways}) -> permanent;
restart_from_ps({restartOnCrash}) -> temporary.

shutdown_from_ps({killImmediately}) -> brutal;
shutdown_from_ps({killNever}) -> infinity;
shutdown_from_ps({killAfter, Ms}) ->  Ms.

type_from_ps({supervisor}) -> supervisor;
type_from_ps({worker}) -> worker.

start_proxy(StartEffect) ->
  StartResult = StartEffect(),
  start_link_result_from_ps(StartResult).
