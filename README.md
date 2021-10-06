# purescript-erl-pinto

Opinionated Bindings to OTP

## Type-safe bindings

Low level bindings to OTP aren't directly user friendly, so this library goes a step up and while a lot of the functionality is recognisable intuitively from the original OTP documentation, the usage is more function-centric so it reads more like an actual Purescript application.

## Define a gen server
```purescript

module MyGenServer where
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer

type EmptyGenServerStartArgs
  = {}

type State
  = {}

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = Local $ atom "my_gen_server"

startLink :: EmptyGenServerStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

doSomething :: Effect String
doSomething = GenServer.call (ByName serverName) (\_from state -> pure $ GenServer.reply "Hi" state)

init :: EmptyGenServerStartArgs -> GenServer.InitFn Unit Unit Unit State
init _args = do
  pure $ InitOk {}


```

## Define a gen supervisor that uses that gen server

```purescript

module MySup where

import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Supervisor

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Sup.startLink (Just $ Local $ atom "my_sup") init

init :: Effect SupervisorSpec
init = do
  pure
    { flags:
        { strategy: OneForOne
        , intensity: 1
        , period: Seconds 5.0
        }
    , childSpecs:
        (spec { id: "cool_worker",
                start: MyGenServer.startLink {},
                childType: Worker,
                restartStrategy: RestartTransient,
                shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
                })
        : nil
    }


```

## Define an application that uses this supervisor

```purescript
module MyApp where

import Pinto.App as App

start = App.simpleStart MySup.startLink
```

## Link to it in an ordinary erlang app.src

```erlang
{application, my_amazing_app,
[{description, "An OTP application"},
{vsn, "0.1.0"},
{registered, []},
{mod, { myApp@ps, []}},
{applications,
[kernel,
stdlib
]}
]}.
```

An end-to-end example can be found in the [demo project](https://github.com/id3as/demo-ps)


Disclaimer
==

This software, and the opinionated libraries written to support it are very much "works in progress" - we are actively using and building these libraries out for use in own commercial software and can and will be making any changes required to further support that development. As such, they come without support and a disclaimer very much of "be it on your own heads". That said - feel free to reach out and talk to us if you have ideas though, improvements and suggestions are welcome in pull requests and conversation.

