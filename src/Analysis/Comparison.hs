module Analysis.Comparison where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeComparisonIssues :: CTranslUnit -> [Issue]
analyzeComparisonIssues ast =
    checkLoopCounterAsIntWhenIteratingOverPtrArrays ast
    ++ checkPtrComparisonWithIntConsts ast
    ++ checkUsingIntForFileOffsets ast

-- loopCounterAsIntWhenIteratingOverPtrArrays
checkLoopCounterAsIntWhenIteratingOverPtrArrays :: CTranslUnit -> [Issue]
checkLoopCounterAsIntWhenIteratingOverPtrArrays ast = []

-- ptrComparisonWithIntConsts
checkPtrComparisonWithIntConsts :: CTranslUnit -> [Issue]
checkPtrComparisonWithIntConsts ast = []

-- usingIntForFileOffsets
checkUsingIntForFileOffsets :: CTranslUnit -> [Issue]
checkUsingIntForFileOffsets ast = []
