module Pinto.Sup where

import Prelude
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, tuple3)
import Erl.Process.Raw (Pid)
import Erl.Atom (Atom, atom)
import Effect (Effect)

import Pinto as Pinto
import Unsafe.Coerce (unsafeCoerce)

foreign import data BoxedStartFn :: Type
foreign import data BoxedStartArgs :: Type

foreign import startLinkImpl :: Atom -> Effect SupervisorSpec -> Effect Pinto.StartLinkResult
foreign import startChildImpl :: forall args. (Pid -> Pinto.StartChildResult) -> (Pid -> Pinto.StartChildResult) -> Atom -> args -> Effect Pinto.StartChildResult

startChild :: forall args. String -> args -> Effect Pinto.StartChildResult
startChild name args = startChildImpl Pinto.AlreadyStarted Pinto.Started (atom name) args

startLink :: String -> Effect SupervisorSpec -> Effect Pinto.StartLinkResult
startLink name spec = startLinkImpl (atom name) spec

data SupervisorStrategy = SimpleOneForOne | OneForOne | OneForAll | RestForOne
data SupervisorChildType = GenSupervisor | GenServer
data SupervisorChildRestart = Transient | Permanent | Temporary                                      
data SupervisorChildShutdown = Infinity | Brutal | Timeout Int

emptyStartFn :: BoxedStartFn
emptyStartFn = unsafeCoerce (\_ -> unit)

emptyStartArgs :: BoxedStartArgs
emptyStartArgs = unsafeCoerce unit

type SupervisorChildSpec =
  { type_ :: SupervisorChildType
  , id :: String
  , startFn :: BoxedStartFn
  , startArgs :: BoxedStartArgs
  , restart :: SupervisorChildRestart
  , shutdown :: SupervisorChildShutdown
  }

type SupervisorSpec =
  { strategy :: SupervisorStrategy
  , intensity :: Int
  , period :: Int
  , children :: List SupervisorChildSpec
  }

type ReifiedSupervisorSpec = Tuple2 ReifiedSupervisorFlags (List ReifiedSupervisorChild)
type ReifiedSupervisorFlags = Tuple3 Atom Int Int
type ReifiedSupervisorChild =
  { id :: Atom
  , start :: Tuple3 Atom Atom (List SupervisorChildSpec)
  , restart :: Atom
  , shutdown :: ReifiedhildShutdown
  , type :: Atom
  }

foreign import data ReifiedhildShutdown :: Type -- Can be { Int or Atom, unsafeCoerce it is )

buildSupervisor :: SupervisorSpec
buildSupervisor =
  { strategy : OneForOne
  , intensity : 10
  , period : 10
  , children : nil
  }

buildChild :: SupervisorChildSpec
buildChild =
  { type_ : GenServer
  , id : "unnamed"
  , startFn : emptyStartFn
  , startArgs: emptyStartArgs
  , shutdown : Timeout 5000
  , restart : Transient
  }

supervisorStrategy :: SupervisorStrategy -> SupervisorSpec  -> SupervisorSpec
supervisorStrategy strategy spec = (spec { strategy = strategy })

supervisorIntensity :: Int -> SupervisorSpec  -> SupervisorSpec
supervisorIntensity intensity spec = (spec { intensity = intensity })

supervisorPeriod :: Int -> SupervisorSpec  -> SupervisorSpec
supervisorPeriod period spec = (spec { period = period })

supervisorChildren :: (List SupervisorChildSpec) -> SupervisorSpec  -> SupervisorSpec
supervisorChildren children spec = (spec { children = children })

childType :: SupervisorChildType -> SupervisorChildSpec -> SupervisorChildSpec
childType type_ spec = (spec { type_ = type_ })

childShutdown :: SupervisorChildShutdown -> SupervisorChildSpec -> SupervisorChildSpec
childShutdown shutdown spec = (spec { shutdown = shutdown })

childId :: String -> SupervisorChildSpec -> SupervisorChildSpec
childId id spec = (spec { id = id })

childStartTemplate :: forall args. (args -> Effect Pinto.StartLinkResult) -> SupervisorChildSpec -> SupervisorChildSpec
childStartTemplate startTemplate spec = (spec { startFn = (unsafeCoerce startTemplate) })

childRestart :: SupervisorChildRestart -> SupervisorChildSpec -> SupervisorChildSpec
childRestart restart spec = (spec { restart = restart })

childStart :: forall args. (args -> Effect Pinto.StartLinkResult) -> args -> SupervisorChildSpec -> SupervisorChildSpec
childStart startFn startArgs spec =
  (spec { startFn = (unsafeCoerce startFn)
        , startArgs = (unsafeCoerce startArgs)
        })


reify :: SupervisorSpec -> ReifiedSupervisorSpec
reify spec = tuple2 (reifySupervisorFlags spec) (reifySupervisorChildren spec)

reifySupervisorFlags :: SupervisorSpec -> ReifiedSupervisorFlags
reifySupervisorFlags spec =
  tuple3 (case spec.strategy of
                  SimpleOneForOne -> (atom "simple_one_for_one")
                  OneForOne -> (atom "one_for_one")
                  OneForAll -> (atom "one_for_all")
                  RestForOne -> (atom "rest_for_one")
         )
         spec.intensity
         spec.period
         
reifySupervisorChildren :: SupervisorSpec -> (List ReifiedSupervisorChild)
reifySupervisorChildren spec =
  map reifySupervisorChild spec.children

reifySupervisorChild :: SupervisorChildSpec -> ReifiedSupervisorChild
reifySupervisorChild spec =
  {
    id : (atom spec.id)
  , start : (case spec.type_ of
                 GenSupervisor -> tuple3 (atom "frameworkSup@foreign") (atom "start_from_spec") (spec : nil)
                 GenServer -> tuple3 (atom "frameworkGen@foreign") (atom "start_from_spec") (spec : nil)
            )
  , restart : (case spec.restart of
                        Transient -> (atom "transient")
                        Permanent -> (atom "permanent")
                        Temporary -> (atom "temporary"))
  , type : (case spec.type_ of
                    GenServer -> (atom "worker")
                    GenSupervisor -> (atom "supervisor"))
  , shutdown : case spec.shutdown of
                          Brutal -> unsafeCoerce $ atom "brutal"
                          Infinity -> unsafeCoerce $ atom "infinity"
                          Timeout value -> unsafeCoerce $ value
  }
