## Module Pinto.Sup

This module defines the means to define a supervisor and its children

For example
```purescript
import Pinto as Pinto
import Pinto.Sup 

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "my_cool_sup" init

init :: Effect SupervisorSpec
init = do
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( ( buildChild
                                       # childType GenServer
                                       # childId "some_child"
                                       # childStart MyGenServer.startLink unit)
                                        : nil)
```

#### `startChild`

``` purescript
startChild :: forall args. String -> args -> Effect StartChildResult
```

Dynamically starts a child with the supplied name and args as specified with the child template
Note: This API is subject to change, as it is "handwavingly" typed and currently maps 1-1 with the 
native Erlang implementation
See also: supervisor:start_child in the OTP docs

#### `startLink`

``` purescript
startLink :: String -> Effect SupervisorSpec -> Effect StartLinkResult
```

Starts a supervisor with the supplied name, using the supplied SupervisorSpec
This is effectful to allow for reading of config/etc
See also: supervisor:start_child in the OTP docs

#### `BoxedStartFn`

``` purescript
data BoxedStartFn :: Type
```

#### `BoxedStartArgs`

``` purescript
data BoxedStartArgs :: Type
```

#### `SupervisorSpec`

``` purescript
type SupervisorSpec = { strategy :: SupervisorStrategy, intensity :: Int, period :: Int, children :: List SupervisorChildSpec }
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `SupervisorStrategy`

``` purescript
data SupervisorStrategy
  = SimpleOneForOne
  | OneForOne
  | OneForAll
  | RestForOne
```

See also supervisor:strategy()
Maps to simple_one_for_one | one_for_one .. etc

#### `SupervisorChildType`

``` purescript
data SupervisorChildType
  = Supervisor
  | Worker
```

Maps to supervisor | worker

#### `SupervisorChildRestart`

``` purescript
data SupervisorChildRestart
  = Transient
  | Permanent
  | Temporary
```

Maps to transient | permanent | temporary

#### `SupervisorChildShutdown`

``` purescript
data SupervisorChildShutdown
  = Infinity
  | Brutal
  | Timeout Int
```

Maps to infinity | brutal | { timeout, N }

#### `SupervisorChildSpec`

``` purescript
type SupervisorChildSpec = { type_ :: SupervisorChildType, id :: String, startFn :: BoxedStartFn, startArgs :: BoxedStartArgs, restart :: SupervisorChildRestart, shutdown :: SupervisorChildShutdown }
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `ReifiedSupervisorSpec`

``` purescript
type ReifiedSupervisorSpec = Tuple2 ReifiedSupervisorFlags (List ReifiedSupervisorChild)
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `ReifiedSupervisorFlags`

``` purescript
type ReifiedSupervisorFlags = Tuple3 Atom Int Int
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `ReifiedSupervisorChild`

``` purescript
type ReifiedSupervisorChild = { id :: Atom, start :: Tuple3 Atom Atom (List SupervisorChildSpec), restart :: Atom, shutdown :: ReifiedhildShutdown, type :: Atom }
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `ReifiedhildShutdown`

``` purescript
data ReifiedhildShutdown :: Type
```

This type is not used directly, but is here to support the buildSupervisor hierarchy

#### `buildSupervisor`

``` purescript
buildSupervisor :: SupervisorSpec
```

Creates a supervisor with default options and no children

#### `buildChild`

``` purescript
buildChild :: SupervisorChildSpec
```

Creates a supervisor child with default options and no actual functionality

#### `supervisorStrategy`

``` purescript
supervisorStrategy :: SupervisorStrategy -> SupervisorSpec -> SupervisorSpec
```

Sets the 'strategy' for the supervisor spec 
See also the OTP docs for supervisor specs

#### `supervisorIntensity`

``` purescript
supervisorIntensity :: Int -> SupervisorSpec -> SupervisorSpec
```

Sets the 'intensity' for the supervisor spec 
See also the OTP docs for supervisor specs

#### `supervisorPeriod`

``` purescript
supervisorPeriod :: Int -> SupervisorSpec -> SupervisorSpec
```

Sets the 'period' for the supervisor spec 
See also the OTP docs for supervisor specs

#### `childType`

``` purescript
childType :: SupervisorChildType -> SupervisorChildSpec -> SupervisorChildSpec
```

Sets the 'child_type' of a child spec
See also the OTP docs for supervisor specs

#### `childShutdown`

``` purescript
childShutdown :: SupervisorChildShutdown -> SupervisorChildSpec -> SupervisorChildSpec
```

Sets the 'shutdown' of a child spec
See also the OTP docs for supervisor specs

#### `childId`

``` purescript
childId :: String -> SupervisorChildSpec -> SupervisorChildSpec
```

Sets the 'id' of a child spec
See also the OTP docs for supervisor specs

#### `childStartTemplate`

``` purescript
childStartTemplate :: forall args. (args -> Effect StartLinkResult) -> SupervisorChildSpec -> SupervisorChildSpec
```

Configures the template for the children started by this supervisor
Again - this API is subject to change as it's "handwavingly" typed, see also Sup.startChild
See also the OTP docs for supervisor specs

#### `childRestart`

``` purescript
childRestart :: SupervisorChildRestart -> SupervisorChildSpec -> SupervisorChildSpec
```

Sets the 'restart' value of a child spec
See also the OTP docs for supervisor specs

#### `childStart`

``` purescript
childStart :: forall args. (args -> Effect StartLinkResult) -> args -> SupervisorChildSpec -> SupervisorChildSpec
```

Sets the callback and args for starting the child 

#### `reify`

``` purescript
reify :: SupervisorSpec -> ReifiedSupervisorSpec
```

Internal function to support the buildSupervisor hierarchy

#### `reifySupervisorFlags`

``` purescript
reifySupervisorFlags :: SupervisorSpec -> ReifiedSupervisorFlags
```

Internal function to support the buildSupervisor hierarchy

#### `reifySupervisorChildren`

``` purescript
reifySupervisorChildren :: SupervisorSpec -> (List ReifiedSupervisorChild)
```

Internal function to support the buildSupervisor hierarchy

#### `reifySupervisorChild`

``` purescript
reifySupervisorChild :: SupervisorChildSpec -> ReifiedSupervisorChild
```

Internal function to support the buildSupervisor hierarchy


