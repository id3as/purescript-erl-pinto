module Pinto.ModuleNames where

import Erl.ModuleName (ModuleName(..))

pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

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
