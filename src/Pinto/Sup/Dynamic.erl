-module(pinto_sup_dynamic@foreign).

%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
-export([ startLinkFFI/2
        , startChildFFI/2
        ]).


%%------------------------------------------------------------------------------
%% Runtime supervisor stubs
%%------------------------------------------------------------------------------
-export([ start_proxy/2
        , init/1
        ]).

-import('pinto_types@foreign',
        [ start_link_result_to_ps/1
        , start_link_result_from_ps/1
        ]).

-import('pinto_sup@foreign',
        [ restart_from_ps/1,
          shutdown_from_ps/1,
          type_from_ps/1
        ]).
init(EffectSupervisorSpec) ->
  DynamicSpecPS = EffectSupervisorSpec(),
  DynamicSpec = dynamic_spec_from_ps(DynamicSpecPS),

  {ok, DynamicSpec}.


%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
startLinkFFI(Name, DynamicSpecEffect) ->
  fun() ->
      startLinkPure(Name, DynamicSpecEffect)
  end.

startLinkPure({nothing}, DynamicSpecEffect) ->
  Result = supervisor:start_link(?MODULE, DynamicSpecEffect),
  start_link_result_to_ps(Result);
startLinkPure({just, RegistryName}, DynamicSpecEffect) ->
  Result = supervisor:start_link(RegistryName, ?MODULE, DynamicSpecEffect),
  start_link_result_to_ps(Result).

startChildFFI(Ref, ChildArg) ->
  fun() ->
    Result = supervisor:start_child(Ref, [ChildArg]),
    start_child_result_to_ps(Result)
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

%%------------------------------------------------------------------------------
%% ps -> erlang conversion helpers
%%------------------------------------------------------------------------------
dynamic_spec_from_ps(#{ intensity := Intensity
                      , period := Period

                      , start := StartFn
                      , restartStrategy := RestartStrategy
                      , shutdownStrategy := ChildShutdownTimeoutStrategy
                      , childType := ChildType
                      }) ->

  SupFlags =
    #{ strategy => simple_one_for_one
     , intensity => Intensity
     , period => Period
     },

  ChildSpec =
    #{ id => dynamic_child
     , start => {?MODULE, start_proxy, [StartFn]}
     , restart => restart_from_ps(RestartStrategy)
     , shutdown => shutdown_from_ps(ChildShutdownTimeoutStrategy)
     , type => type_from_ps(ChildType)
     },

    { SupFlags, [ ChildSpec ] }.



start_proxy(StartFn, StartArg) ->
  StartEffect = StartFn(StartArg),
  StartResult = StartEffect(),
  start_link_result_from_ps(StartResult).
