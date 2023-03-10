-- | This module wraps erlang:monitor in `Pinto.MessageRouting` so that sensible message structures can be sent
-- | back to the monitoring process
module Pinto.Monitor
  ( monitor
  , monitorTo
  , demonitor
  , MonitorMsg(..)
  , MonitorType(..)
  , MonitorRef
  , MonitorObject
  , MonitorInfo
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Pinto.MessageRouting as MR
import Erl.Process.Raw (Pid, class HasPid, getPid)
import Erl.Process (class HasSelf, send, self, class HasProcess, getProcess)

-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type

-- | This is probably a Pid, but until it is needed it will be Foreign
type MonitorObject = Foreign

-- | The 'reason' for the monitor being invoked, if this needs unpacking
-- | then FFI will need to be written
type MonitorInfo = Foreign

-- | The type of monitor this message is being sent on behalf
data MonitorType
  = Process
  | Port

-- | Reference to a monitor, used to stop the monitor once it is started
data MonitorMsg = Down (MR.RouterRef MonitorRef) MonitorType MonitorObject MonitorInfo

-- | Given something that has a pid (A GenServer, a Process.. or just a Pid), attach a monitor by using
-- | erlang:monitor on the underlying pid, a message will be sent to the current process, lifted into the
-- | constructor `f` provided
monitor
  :: forall msg process m
   . HasPid process
  => MonadEffect m
  => HasSelf m msg
  => process
  -> (MonitorMsg -> msg)
  -> m (MR.RouterRef MonitorRef)
monitor process f = do
  me <- self
  liftEffect $ MR.startRouter (startMonitor $ getPid process) (\_ -> stopMonitor) (handleMessage me) unit
  where
  handleMessage me _ msg = do
    _ <- handleMonitorMessage Down (send me <<< f) msg
    _ <- MR.stopRouterFromCallback
    pure unit

-- | Given something that has a pid (A GenServer, a Process.. or just a Pid), attach a monitor by using
-- | erlang:monitor on the underlying pid, a message will be sent to the current process, lifted into the
-- | constructor `f` provided
monitorTo
  :: forall msg process target
   . HasPid process
  => HasProcess msg target
  => process
  -> target
  -> (MonitorMsg -> msg)
  -> Effect (MR.RouterRef MonitorRef)
monitorTo process target f = do
  let p = getProcess target
  MR.startRouter (startMonitor $ getPid process) (\_ -> stopMonitor) (handleMessage p) unit
  where
  handleMessage target' _ msg = do
    _ <- handleMonitorMessage Down (send target' <<< f) msg
    _ <- MR.stopRouterFromCallback
    pure unit

-- | Stops a monitor started with Monitor.monitor, using erlang:demonitor and subject to the same restrictions/caveats
demonitor :: MR.RouterRef MonitorRef -> Effect Unit
demonitor = MR.stopRouter

foreign import startMonitor :: Pid -> Effect MonitorRef
foreign import stopMonitor :: MonitorRef -> Effect Unit

foreign import handleMonitorMessage
  :: forall msg
   . (MR.RouterRef MonitorRef -> MonitorType -> MonitorObject -> MonitorInfo -> MonitorMsg)
  -> (MonitorMsg -> Effect Unit)
  -> msg
  -> Effect Unit
