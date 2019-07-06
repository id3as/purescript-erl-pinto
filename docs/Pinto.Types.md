## Module Pinto.Types

#### `ServerName`

``` purescript
data ServerName state
  = ServerName String
```

Defines the server name for a gen server, along with the 'state' that the gen server
will be using internally - this will be supplied to every call to the gen server API in order
to enforce type safety across calls

#### `StartLinkResult`

``` purescript
type StartLinkResult = Tuple2 Atom Pid
```

The result of invoking gen_server:start_link

#### `StartChildResult`

``` purescript
data StartChildResult
  = AlreadyStarted Pid
  | Started Pid
```

The result of supervisor:start_child


