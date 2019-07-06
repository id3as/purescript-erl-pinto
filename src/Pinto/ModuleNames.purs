module ModuleNames where

import Erl.Atom (atom)
import Erl.ModuleName (ModuleName(..))


pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

pintoTimer :: ModuleName
pintoTimer = ModuleName "Pinto.Timer"

pintoGen :: ModuleName
pintoGen = ModuleName "Pinto.Gen"

pintoTypes :: ModuleName
pintoTypes = ModuleName "Pinto.Types"

pintoSup :: ModuleName
pintoSup = ModuleName "Pinto.Sup"

moduleNames :: ModuleName
moduleNames = ModuleName "ModuleNames"
