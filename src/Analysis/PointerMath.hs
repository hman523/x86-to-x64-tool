module Analysis.PointerMath where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzePointerMathIssues :: CTranslUnit -> [Issue]
analyzePointerMathIssues ast = 
    checkPtrDiffStoredAs32bit ast
    ++ checkPtrAddOverflow ast
    ++ checkPtrSubUnderflow ast
    ++ checkArrayIndexingIntInArrayOver2tothe31size ast

-- ptrDiffStoredAs32bit
checkPtrDiffStoredAs32bit :: CTranslUnit -> [Issue]
checkPtrDiffStoredAs32bit ast = []

-- ptrAddOverflow
checkPtrAddOverflow :: CTranslUnit -> [Issue]
checkPtrAddOverflow ast = []

-- ptrSubUnderflow
checkPtrSubUnderflow :: CTranslUnit -> [Issue]
checkPtrSubUnderflow ast = []

-- ArrayIndexingIntInArrayOver2tothe31size
checkArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> [Issue]
checkArrayIndexingIntInArrayOver2tothe31size ast = []
