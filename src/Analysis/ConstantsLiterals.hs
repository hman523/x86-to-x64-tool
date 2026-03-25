module Analysis.ConstantsLiterals where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeConstantsLiteralsIssues :: CTranslUnit -> [Issue]
analyzeConstantsLiteralsIssues ast =
    checkMagicValuesUsed ast
    ++ checkBitMaskingAssuming32bitPts ast
    ++ checkHardCodedAddressValues ast
    ++ checkConstantsUsedForSizeCalcs ast

-- magicValuesUsed
checkMagicValuesUsed :: CTranslUnit -> [Issue]
checkMagicValuesUsed ast = []

-- bitMaskingAssuming32bitPts
checkBitMaskingAssuming32bitPts :: CTranslUnit -> [Issue]
checkBitMaskingAssuming32bitPts ast = []

-- hardCodedAddressValues
checkHardCodedAddressValues :: CTranslUnit -> [Issue]
checkHardCodedAddressValues ast = []

-- constantsUsedForSizeCalcs
checkConstantsUsedForSizeCalcs :: CTranslUnit -> [Issue]
checkConstantsUsedForSizeCalcs ast = []