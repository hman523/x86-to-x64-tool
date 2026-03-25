module Analysis.FunctionSignatures where 

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeFunctionSignatureIssues :: CTranslUnit -> [Issue]
analyzeFunctionSignatureIssues ast =
    checkFnsReturnPtrAsInt ast
    ++ checkFnsReturnPtrAsLong ast
    ++ checkFnsParamDeclaredAsIntTakesPtr ast
    ++ checkVaargUsingWrongTypesForPtrArgs ast


-- fnsReturnPtrAsInt
checkFnsReturnPtrAsInt :: CTranslUnit -> [Issue]
checkFnsReturnPtrAsInt ast = []

-- fnsReturnPtrAsLong
checkFnsReturnPtrAsLong :: CTranslUnit -> [Issue]
checkFnsReturnPtrAsLong ast = []

-- fnsParamDeclaredAsIntTakesPtr
checkFnsParamDeclaredAsIntTakesPtr :: CTranslUnit -> [Issue]
checkFnsParamDeclaredAsIntTakesPtr ast = []

-- vaargUsingWrongTypesForPtrArgs
checkVaargUsingWrongTypesForPtrArgs :: CTranslUnit -> [Issue]
checkVaargUsingWrongTypesForPtrArgs ast = []