## Module Pinto.Timer

This module provides a means of using the timer functionality in core Erlang
This is subject to change as currently it only works within the context of a genserver

#### `sendEvery`

``` purescript
sendEvery :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
```

Invokes the supplied callback within the context of the current gen server every
N milliseconds (This API may be killed in the near future, as this is a silly way to do this)
See also timer:send_every in the OTP docs

#### `sendAfter`

``` purescript
sendAfter :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
```

Invokes the supplied callback within the context of the current gen server after
N milliseconds (This API may be killed in the near future, as this is a silly way to do this)
See also timer:send_after in the OTP docs

#### `cancel`

``` purescript
cancel :: TimerRef -> Effect Unit
```

Cancels a timer started by either sendEvery or sendAfter
(This API may be killed in the near future, as this is a silly way to do this)
See also timer:cancel in the OTP docs

#### `TimerRef`

``` purescript
newtype TimerRef
```


