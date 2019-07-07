# purescript-erl-pinto

Opinionated Bindings to OTP

## Type-safe bindings

Low level bindings to OTP aren't directly user friendly, so this library goes a step up and while a lot of the functionality is recognisable intuitively from the original OTP documentation, the usage is more function-centric so it reads more like an actual Purescript application.

## Define a gen server
```purescript

module MyGenServer where

import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen

type State = { }

serverName :: ServerName State
serverName = ServerName "some_uuid"

doSomething :: Unit -> Effect Unit
doSomething input = 
  Gen.doCall serverName \state -> do
    pure $ CallReply unit state

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: Unit -> Effect State
init args = do
  pure $ {}
```

## Define a gen supervisor that uses that gen server

```purescript

module MyGenSup where

import Pinto as Pinto
import Pinto.Sup 

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "my_cool_sup" init

init :: Effect SupervisorSpec
init = do
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( ( buildChild
                                       # childType Worker
                                       # childId "some_child"
                                       # childStart MyGenServer.startLink unit)
                                        : nil)

```

## Define an application that uses this supervisor

```purescript
import Pinto.App as App

start = App.simpleStart MyGenSup.startLink
```

## Link to it in an ordinary erlang app.src

```erlang
{application, my_amazing_app,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { bookApp@ps, []}},
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

