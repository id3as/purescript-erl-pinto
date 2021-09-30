-- | This module wraps erlang:monitor in `Pinto.MessageRouting` so that sensible message structures can be sent
-- | back to the monitoring process
module Pinto.Monitor
  ( monitor
  , demonitor
  , MonitorMsg(..)
  , MonitorType(..)
  , MonitorRef
  , MonitorObject
  , MonitorInfo
  ) where

import Prelude
import Effect (Effect)
import Foreign (Foreign)
import Pinto.MessageRouting as MR
import Erl.Process.Raw (Pid, class HasPid, getPid)

-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type

-- | This is probably a Pid, but until it is needed it will be Foreign
type MonitorObject
  = Foreign

-- | The 'reason' for the monitor being invoked, if this needs unpacking
-- | then FFI will need to be written
type MonitorInfo
  = Foreign

-- | The type of monitor this message is being sent on behalf
data MonitorType
  = Process
  | Port

-- | Reference to a monitor, used to stop the monitor once it is started
data MonitorMsg
  = Down (MR.RouterRef MonitorRef) MonitorType MonitorObject MonitorInfo

-- | Given something that has a pid (A GenServer, a Process.. or just a Pid), attach a monitor by using
-- | erlang:monitor on the underlying pid, the supplied callback will be invoked should the monitor fire
monitor :: forall process. HasPid process => process -> (MonitorMsg -> Effect Unit) -> Effect (MR.RouterRef MonitorRef)
monitor process cb = MR.startRouter (startMonitor $ getPid process) stopMonitor handleMessage
  where
  handleMessage msg = do
    _ <- handleMonitorMessage Down cb msg
    _ <- MR.stopRouterFromCallback
    pure unit

-- | Stops a monitor started with Monitor.monitor, using erlang:demonitor and subject to the same restrictions/caveats
demonitor :: MR.RouterRef MonitorRef -> Effect Unit
demonitor = MR.stopRouter

foreign import startMonitor :: Pid -> Effect MonitorRef
foreign import stopMonitor :: MonitorRef -> Effect Unit

foreign import handleMonitorMessage ::
  forall msg.
  (MR.RouterRef MonitorRef -> MonitorType -> MonitorObject -> MonitorInfo -> MonitorMsg) ->
  (MonitorMsg -> Effect Unit) ->
  msg ->
  Effect Unit
