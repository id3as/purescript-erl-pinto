module Pinto.ModuleNames where

import Erl.ModuleName (ModuleName(..))

pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

pintoModuleNames :: ModuleName
pintoModuleNames = ModuleName "Pinto.ModuleNames"

pintoSup :: ModuleName
pintoSup = ModuleName "Pinto.Sup"

pintoTypes :: ModuleName
pintoTypes = ModuleName "Pinto.Types"
