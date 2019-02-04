module ModuleNames where

import Erl.Atom (atom)
import Erl.ModuleName (ModuleName(..))


pinto :: ModuleName
pinto = ModuleName "Pinto"

pintoApp :: ModuleName
pintoApp = ModuleName "Pinto.App"

pintoGen :: ModuleName
pintoGen = ModuleName "Pinto.Gen"

pintoSup :: ModuleName
pintoSup = ModuleName "Pinto.Sup"

pintoTimer :: ModuleName
pintoTimer = ModuleName "Pinto.Timer"

pintoTypes :: ModuleName
pintoTypes = ModuleName "Pinto.Types"

moduleNames :: ModuleName
moduleNames = ModuleName "ModuleNames"
