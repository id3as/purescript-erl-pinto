module Pinto.ModuleNames where

import Erl.ModuleName (ModuleName(..))

pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

pintoGenServer :: ModuleName
pintoGenServer = ModuleName "Pinto.GenServer"

pintoGenServerCS :: ModuleName
pintoGenServerCS = ModuleName "Pinto.GenServer.ContStop"

pintoGenStatem :: ModuleName
pintoGenStatem = ModuleName "Pinto.GenStatem"

pintoMessageRouting :: ModuleName
pintoMessageRouting = ModuleName "Pinto.MessageRouting"

pintoModuleNames :: ModuleName
pintoModuleNames = ModuleName "Pinto.ModuleNames"

pintoMonitor :: ModuleName
pintoMonitor = ModuleName "Pinto.Monitor"

pintoSup :: ModuleName
pintoSup = ModuleName "Pinto.Sup"

pintoTypes :: ModuleName
pintoTypes = ModuleName "Pinto.Types"
