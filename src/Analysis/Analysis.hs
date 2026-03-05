module Analysis.Analysis where 

import Language.C.Syntax.AST
import Analysis.UtilTypes
import Analysis.TypeSize

analysis :: CTranslUnit -> [Issue]
analysis = undefined