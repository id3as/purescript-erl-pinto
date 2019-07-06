module Pinto.ModuleNames where

import Erl.ModuleName (ModuleName(..))

pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

pintoGen :: ModuleName
pintoGen = ModuleName "Pinto.Gen"

pintoModuleNames :: ModuleName
pintoModuleNames = ModuleName "Pinto.ModuleNames"

pintoSup :: ModuleName
pintoSup = ModuleName "Pinto.Sup"

pintoTimer :: ModuleName
pintoTimer = ModuleName "Pinto.Timer"

pintoTypes :: ModuleName
pintoTypes = ModuleName "Pinto.Types"
