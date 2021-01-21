module Pinto.Monitor
       ( pid
       , process
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
import Pinto.Types (ServerPid)

foreign import startMonitor :: forall serverType. ServerPid serverType -> Effect MonitorRef
foreign import stopMonitor :: MonitorRef -> Effect Unit
foreign import handleMonitorMessage :: forall msg. (MR.RouterRef MonitorRef -> MonitorType -> MonitorObject -> MonitorInfo -> MonitorMsg) -> (MonitorMsg -> Effect Unit) -> msg -> Effect Unit

-- Doing this lazy for now, stand-in types
-- If you find yourself needing this data then the 'correct' thing to do
-- will be to create real types for them and pass in functions  for their construction to the Erlang code
foreign import data MonitorRef :: Type
type MonitorObject = Foreign
type MonitorInfo =  Foreign

data MonitorType = Process | Port

data MonitorMsg = Down (MR.RouterRef MonitorRef) MonitorType MonitorObject MonitorInfo

pid :: forall serverType. ServerPid serverType -> (MonitorMsg -> Effect Unit) -> Effect (MR.RouterRef MonitorRef)
pid p cb =
  MR.startRouter (startMonitor p) stopMonitor $ (\msg -> do
                                                      _ <- handleMonitorMessage Down cb msg
                                                      _ <- MR.stopRouterFromCallback
                                                      pure unit)

process :: forall serverType. ServerPid serverType -> (MonitorMsg -> Effect Unit) -> Effect (MR.RouterRef MonitorRef)
process p cb =
  MR.startRouter (startMonitor p) stopMonitor $ (\msg -> do
                                                      _ <- handleMonitorMessage Down cb msg
                                                      _ <- MR.stopRouterFromCallback
                                                      pure unit)

demonitor :: MR.RouterRef MonitorRef -> Effect Unit
demonitor = MR.stopRouter
