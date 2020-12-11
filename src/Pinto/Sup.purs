-- | This module defines the means to define a supervisor and its children
-- |
-- | For example
-- | ```purescript
-- | import Pinto as Pinto
-- | import Pinto.Sup
-- |
-- | startLink :: Effect Pinto.StartLinkResult
-- | startLink = Sup.startLink "my_cool_sup" init
-- |
-- | init :: Effect SupervisorSpec
-- | init = do
-- |   pure $ buildSupervisor
-- |                 # supervisorStrategy OneForOne
-- |                 # supervisorChildren ( ( buildChild
-- |                                        # childType GenServer
-- |                                        # childId "some_child"
-- |                                        # childStart MyGenServer.startLink unit)
-- |                                         : nil)
-- | ```
module Pinto.Sup ( startSimpleChild
                 , startSpeccedChild
                 , terminateChild
                 , deleteChild
                 , stop
                 , startLink
                 , BoxedStartFn
                 , BoxedStartArgs
                 , SupervisorSpec(..)
                 , SupervisorStrategy(..)
                 , SupervisorChildType(..)
                 , SupervisorChildRestart(..)
                 , SupervisorChildShutdown(..)
                 , SupervisorChildSpec
                 , ReifiedSupervisorSpec
                 , ReifiedSupervisorFlags
                 , ReifiedSupervisorChild
                 , ReifiedChildShutdown
                 , buildSupervisor
                 , buildChild
                 , supervisorStrategy
                 , supervisorIntensity
                 , supervisorPeriod
                 , supervisorChildren
                 , childType
                 , childShutdown
                 , childId
                 , childStartTemplate
                 , childRestart
                 , childStart
                 , reify
                 , reifySupervisorFlags
                 , reifySupervisorChildren
                 , reifySupervisorChild
                 , slrToForeign
                 , foreignToSlr
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Pinto as Pinto
import Pinto.Types (ChildTemplate(..), ServerName(..), SupervisorName)
import Unsafe.Coerce (unsafeCoerce)

foreign import data BoxedStartFn :: Type
foreign import data BoxedStartArgs :: Type

foreign import startLinkImpl :: forall name state. name -> Effect state -> Effect Pinto.StartLinkResult
foreign import startChildImpl :: forall name args. name -> args -> Effect Pinto.StartChildResult
foreign import startSpeccedChildImpl :: forall name args. (Pid -> Pinto.StartChildResult) -> (Pid -> Pinto.StartChildResult) -> name -> args -> Effect Pinto.StartChildResult
foreign import stopImpl :: forall state name. name -> Effect Unit

foreign import terminateChildImpl :: forall name args. name -> Atom -> Effect Unit
foreign import deleteChildImpl :: forall name args. name -> Atom -> Effect Unit

foreign import foreignToSlr :: Foreign  -> Pinto.StartLinkResult
foreign import foreignToScr :: Foreign  -> Pinto.StartChildResult

foreign import slrToForeign :: Pinto.StartLinkResult -> Foreign
foreign import scrToForeign :: Pinto.StartChildResult -> Foreign


-- These imports are just so we don't get warnings
foreign import init :: forall a. a -> a
foreign import start_from_spec :: forall a. a -> a

-- | Starts a supervisor with the supplied name, using the supplied SupervisorSpec
-- | This is effectful to allow for reading of config/etc
-- | See also: supervisor:start_child in the OTP docs
startLink :: SupervisorName -> Effect SupervisorSpec -> Effect Pinto.StartLinkResult
startLink (Local name) = startLinkImpl $ tuple2 (atom "local") name
startLink (Global name) = startLinkImpl $ tuple2 (atom "global") name
startLink (Via (NativeModuleName m) name) = startLinkImpl $ tuple3 (atom "via") m name

-- | Dynamically starts a child with the supplied name and args as specified with the child template
-- | See also: supervisor:start_child in the OTP docs
startSimpleChild :: forall args state msg. Pinto.ChildTemplate args -> SupervisorHandle state msg -> Effect Pinto.StartChildResult
startSimpleChild _ name args = startChildImpl (nativeName name) args

-- | Dynamically starts a child with the supplied spec
-- | See also: supervisor:start_child in the OTP docs
startSpeccedChild :: SupervisorName -> SupervisorChildSpec  -> Effect Pinto.StartChildResult
startSpeccedChild name spec = startSpeccedChildImpl Pinto.ChildAlreadyStarted Pinto.ChildStarted (nativeName name) $ reifySupervisorChild spec

terminateChild :: SupervisorName -> String  -> Effect Unit
terminateChild name child = terminateChildImpl (nativeName name) (atom child)

deleteChild :: SupervisorName -> String  -> Effect Unit
deleteChild name child = deleteChildImpl (nativeName name) (atom child)

stop :: SupervisorName -> Effect Unit
stop name = stopImpl (nativeName name)

nativeName :: SupervisorName -> Foreign
nativeName (Local name) = unsafeToForeign $ name
nativeName (Global name) = unsafeToForeign $ tuple2 (atom "global") name
nativeName (Via (NativeModuleName m) name) = unsafeToForeign $ tuple3 (atom "via") m name

-- | See also supervisor:strategy()
-- | Maps to simple_one_for_one | one_for_one .. etc
data SupervisorStrategy = SimpleOneForOne | OneForOne | OneForAll | RestForOne

-- | Maps to supervisor | worker
data SupervisorChildType = Supervisor
                         | Worker
                         | NativeSupervisor NativeModuleName Atom (List Foreign)
                         | NativeWorker NativeModuleName Atom (List Foreign)

-- | Maps to transient | permanent | temporary
data SupervisorChildRestart = Transient | Permanent | Temporary

-- | Maps to infinity | brutal | { timeout, N }
data SupervisorChildShutdown = Infinity | Brutal | Timeout Int

emptyStartFn :: BoxedStartFn
emptyStartFn = unsafeCoerce (\_ -> unit)

emptyStartArgs :: BoxedStartArgs
emptyStartArgs = unsafeCoerce unit

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
type SupervisorChildSpec =
  { type_ :: SupervisorChildType
  , id :: String
  , startFn :: BoxedStartFn
  , startArgs :: BoxedStartArgs
  , restart :: SupervisorChildRestart
  , shutdown :: SupervisorChildShutdown
  }

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
type SupervisorSpec =
  { strategy :: SupervisorStrategy
  , intensity :: Int
  , period :: Int
  , children :: List SupervisorChildSpec
  }

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
type ReifiedSupervisorSpec = Tuple2 ReifiedSupervisorFlags (List ReifiedSupervisorChild)

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
type ReifiedSupervisorFlags = Tuple3 Atom Int Int

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
type ReifiedSupervisorChild =
  { id :: Atom
  , start :: Tuple3 Atom Atom (List Foreign)
  , restart :: Atom
  , shutdown :: ReifiedChildShutdown
  , type :: Atom
  }

-- | This type is not used directly, but is here to support the buildSupervisor hierarchy
foreign import data ReifiedChildShutdown :: Type -- Can be { Int or Atom, unsafeCoerce it is )

-- | Creates a supervisor with default options and no children
buildSupervisor :: SupervisorSpec
buildSupervisor =
  { strategy : OneForOne
  , intensity : 10
  , period : 10
  , children : nil
  }


-- | Sets the 'strategy' for the supervisor spec
-- | See also the OTP docs for supervisor specs
supervisorStrategy :: SupervisorStrategy -> SupervisorSpec  -> SupervisorSpec
supervisorStrategy strategy spec = (spec { strategy = strategy })

-- | Sets the 'intensity' for the supervisor spec
-- | See also the OTP docs for supervisor specs
supervisorIntensity :: Int -> SupervisorSpec  -> SupervisorSpec
supervisorIntensity intensity spec = (spec { intensity = intensity })

-- | Sets the 'period' for the supervisor spec
-- | See also the OTP docs for supervisor specs
supervisorPeriod :: Int -> SupervisorSpec  -> SupervisorSpec
supervisorPeriod period spec = (spec { period = period })

-- | Provide a list of child specs for this supervisor structure
supervisorChildren :: (List SupervisorChildSpec) -> SupervisorSpec  -> SupervisorSpec
supervisorChildren children spec = (spec { children = children })

-- | Creates a supervisor child with default options and no actual functionality
buildChild :: SupervisorChildSpec
buildChild =
  { type_ : Worker
  , id : "unnamed"
  , startFn : emptyStartFn
  , startArgs: emptyStartArgs
  , shutdown : Timeout 5000
  , restart : Permanent
  }

-- | Sets the 'child_type' of a child spec
-- | See also the OTP docs for supervisor specs
childType :: SupervisorChildType -> SupervisorChildSpec -> SupervisorChildSpec
childType type_ spec = (spec { type_ = type_ })

-- | Sets the 'shutdown' of a child spec
-- | See also the OTP docs for supervisor specs
childShutdown :: SupervisorChildShutdown -> SupervisorChildSpec -> SupervisorChildSpec
childShutdown shutdown spec = (spec { shutdown = shutdown })

-- | Sets the 'id' of a child spec
-- | See also the OTP docs for supervisor specs
childId :: String -> SupervisorChildSpec -> SupervisorChildSpec
childId id spec = (spec { id = id })

-- | Configures the template for the children started by this supervisor
-- | See also the OTP docs for supervisor specs
childStartTemplate :: forall args. Pinto.ChildTemplate args -> SupervisorChildSpec -> SupervisorChildSpec
childStartTemplate (ChildTemplate startFn) spec = (spec { startFn = (unsafeCoerce (\args -> slrToForeign <$> startFn args)) })

-- | Sets the 'restart' value of a child spec
-- | See also the OTP docs for supervisor specs
childRestart :: SupervisorChildRestart -> SupervisorChildSpec -> SupervisorChildSpec
childRestart restart spec = (spec { restart = restart })

-- | Sets the callback and args for starting the child
childStart :: forall args. (args -> Effect Pinto.StartLinkResult) -> args -> SupervisorChildSpec -> SupervisorChildSpec
childStart startFn startArgs spec =
  (spec { startFn  =  unsafeCoerce $  (\args -> slrToForeign <$> startFn args)
        , startArgs = (unsafeCoerce startArgs)
        })

-- | Internal function to support the buildSupervisor hierarchy
reify :: SupervisorSpec -> ReifiedSupervisorSpec
reify spec = tuple2 (reifySupervisorFlags spec) (reifySupervisorChildren spec)

-- | Internal function to support the buildSupervisor hierarchy
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

-- | Internal function to support the buildSupervisor hierarchy
reifySupervisorChildren :: SupervisorSpec -> (List ReifiedSupervisorChild)
reifySupervisorChildren spec =
  map reifySupervisorChild spec.children

-- | Internal function to support the buildSupervisor hierarchy
reifySupervisorChild :: SupervisorChildSpec -> ReifiedSupervisorChild
reifySupervisorChild spec =
  {
    id : (atom spec.id)
  , start : case spec.type_ of
              Supervisor -> tuple3 (atom "pinto_sup@foreign") (atom "start_from_spec") (unsafeToForeign spec : nil)
              Worker -> tuple3 (atom "pinto_gen@foreign") (atom "start_from_spec") (unsafeToForeign spec : nil)
              NativeSupervisor (NativeModuleName moduleName) startFn args -> tuple3 moduleName startFn args
              NativeWorker (NativeModuleName moduleName) startFn args -> tuple3 moduleName startFn args
  , restart : case spec.restart of
                Transient -> (atom "transient")
                Permanent -> (atom "permanent")
                Temporary -> (atom "temporary")
  , type : case spec.type_ of
             Supervisor -> (atom "supervisor")
             Worker -> (atom "worker")
             NativeSupervisor _ _ _ -> (atom "supervisor")
             NativeWorker _ _ _ -> (atom "worker")
  , shutdown : case spec.shutdown of
                 Brutal -> unsafeCoerce $ atom "brutal"
                 Infinity -> unsafeCoerce $ atom "infinity"
                 Timeout value -> unsafeCoerce $ value
  }
