-module(pinto_sup@foreign).

%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
-export([ mkErlChildSpec/1
        , startLink/2
        ]).


%%------------------------------------------------------------------------------
%% Runtime supervisor stubs
%%------------------------------------------------------------------------------
-export([ start_stub/1
        , init/1
        ]).

init(EffectSupervisorSpec) ->
  #{ flags := Flags
   , childSpecs := ChildSpecs
   } = EffectSupervisorSpec(),
  {ok, {flags_from_ps(Flags), ChildSpecs}}.


%%------------------------------------------------------------------------------
%% FFI API
%%------------------------------------------------------------------------------
mkErlChildSpec(#{ id := ChildId
                , start := StartFn
                , restartStrategy := RestartStrategy
                , shutdownStrategy := ChildShutdownTimeoutStrategy
                , childType := ChildType
                }) ->
  #{ id => ChildId
   , start => {?MODULE, start_stub, [StartFn]}
   , restart => restart_from_ps(RestartStrategy)
   , shutdown => shutdown_from_ps(ChildShutdownTimeoutStrategy)
   , type => type_from_ps(ChildType)
   }.


startLink(Name, EffectSupervisorSpec) ->
  fun() ->
      startLinkPure(Name, EffectSupervisorSpec)
  end.

startLinkPure({nothing}, _EffectSupervisorSpec) ->
  ok;
startLinkPure({just, RegisterName}, EffectSupervisorSpec) ->
  supervisor:start_link(register_name_from_ps(RegisterName), ?MODULE, EffectSupervisorSpec),
  ok.



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






register_name_from_ps({local, Name}) -> Name;
register_name_from_ps({global, _} = Global) -> Global;
register_name_from_ps({via, _, _} = Via) -> Via.


restart_from_ps({restartNever}) -> transient;
restart_from_ps({restartAlways}) -> permanent;
restart_from_ps({restartOnCrash}) -> temporary.

shutdown_from_ps({killImmediately}) -> brutal;
shutdown_from_ps({killNever}) -> infinity;
shutdown_from_ps({killAfter, Ms}) ->  Ms.

type_from_ps({supervisor}) -> supervisor;
type_from_ps({worker}) -> worker.


start_stub(StartFn) ->
  io:format(user, "Start called ~p~n", [StartFn]).
