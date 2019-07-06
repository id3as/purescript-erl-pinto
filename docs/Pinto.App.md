## Module Pinto.App

#### `simpleStart`

``` purescript
simpleStart :: forall args. Effect StartLinkResult -> EffectFn2 Atom (List args) StartLinkResult
```

Defines the entry point to an applicaiton that ignores any passed in arguments and simply calls the supervisor callback provided

For example:

```purescript
App.simpleStart MyGenSup.startLink
```


