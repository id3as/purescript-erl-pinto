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
import Pinto.Types (class HasRawPid, getRawPid)
import Erl.Process.Raw (Pid)

foreign import startMonitor :: Pid -> Effect MonitorRef
foreign import stopMonitor :: MonitorRef -> Effect Unit
foreign import handleMonitorMessage ::
  forall msg.
  (MR.RouterRef MonitorRef -> MonitorType -> MonitorObject -> MonitorInfo -> MonitorMsg) ->
  (MonitorMsg -> Effect Unit)
  -> msg
  -> Effect Unit

-- Doing this lazy for now, stand-in types
-- If you find yourself needing this data then the 'correct' thing to do
-- will be to create real types for them and pass in functions  for their construction to the Erlang code
foreign import data MonitorRef :: Type
type MonitorObject = Foreign
type MonitorInfo =  Foreign

data MonitorType = Process | Port

data MonitorMsg = Down (MR.RouterRef MonitorRef) MonitorType MonitorObject MonitorInfo

monitor :: forall process. HasRawPid process => process -> (MonitorMsg -> Effect Unit) -> Effect (MR.RouterRef MonitorRef)
monitor process cb =
  MR.startRouter (startMonitor $ getRawPid process) stopMonitor handleMessage

  where
    handleMessage msg = do
      _ <- handleMonitorMessage Down cb msg
      _ <- MR.stopRouterFromCallback
      pure unit

demonitor :: MR.RouterRef MonitorRef -> Effect Unit
demonitor = MR.stopRouter
