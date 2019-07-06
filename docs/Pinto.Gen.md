## Module Pinto.Gen

#### `startLink`

``` purescript
startLink :: forall state. ServerName state -> Effect state -> Effect StartLinkResult
```

Starts a typed gen-server proxy with the supplied ServerName, with the state being the result of the supplied effect

```purescript
serverName :: ServerName State
serverName = ServerName "some_uuid"

startLink :: Effect StartLinkResults
startLink = Gen.startLink serverName init

init :: Effect State
init = pure {}
```
See also: gen_server:start_link in the OTP docs

#### `CallResult`

``` purescript
data CallResult response state
  = CallReply response state
  | CallReplyHibernate response state
  | CallStop response state
```

#### `CastResult`

``` purescript
data CastResult state
  = CastNoReply state
  | CastNoReplyHibernate state
  | CastStop state
```

#### `call`

``` purescript
call :: forall response state. ServerName state -> (state -> (CallResult response state)) -> Effect response
```

Defines a "pure" "call" that performs an interaction on the state held by the gen server, but with no other side effects
Directly returns the result of the callback provided
```purescript

doSomething :: Effect Unit
doSomething = Gen.call serverName \state -> CallReply unit (modifyState state)
```
See also handle_call and gen_server:call in the OTP docs

#### `doCall`

``` purescript
doCall :: forall response state. ServerName state -> (state -> Effect (CallResult response state)) -> Effect response
```

Defines an effectful call that performs an interaction on the state held by the gen server, and perhaps side-effects
Directly returns the result of the callback provided
```purescript

doSomething :: Effect Unit
doSomething = Gen.doCall serverName \state -> pure $ CallReply unit (modifyState state)
```
See also handle_call and gen_server:call in the OTP docs

#### `cast`

``` purescript
cast :: forall state. ServerName state -> (state -> (CastResult state)) -> Effect Unit
```

Defines an "pure" cast that performs an interaction on the state held by the gen server
```purescript
doSomething :: Effect Unit
doSomething = Gen.cast serverName \state -> CastNoReply $ modifyState state
```
See also handle_cast and gen_server:cast in the OTP docs

#### `doCast`

``` purescript
doCast :: forall state. ServerName state -> (state -> Effect (CastResult state)) -> Effect Unit
```

Defines an effectful cast that performs an interaction on the state held by the gen server
```purescript
doSomething :: Effect Unit
doSomething = Gen.cast serverName \state -> pure $ CastNoReply $ modifyState state
```
See also handle_cast and gen_server:cast in the OTP docs


