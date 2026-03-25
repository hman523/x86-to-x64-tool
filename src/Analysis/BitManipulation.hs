module Analysis.BitManipulation where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeBitManipulationIssues :: CTranslUnit -> [Issue]
analyzeBitManipulationIssues ast =
    checkPackingPtrsWithFlagsInInt ast
    ++ checkBitShiftsOnPtr ast
    ++ checkExtractingPtrBitsIn32BitVar ast

-- packingPtrsWithFlagsInInt
checkPackingPtrsWithFlagsInInt :: CTranslUnit -> [Issue]
checkPackingPtrsWithFlagsInInt ast = []

-- bitShiftsOnPtr
checkBitShiftsOnPtr :: CTranslUnit -> [Issue]
checkBitShiftsOnPtr ast = []

-- extractingPtrBitsIn32BitVar
checkExtractingPtrBitsIn32BitVar :: CTranslUnit -> [Issue]
checkExtractingPtrBitsIn32BitVar ast = []