module Analysis.TypeSize where 

import Language.C.Syntax.AST
import Language.C.Data.Node
import Analysis.UtilTypes
import qualified Data.Map as Map


analyzeTypeSizeIssues :: CTranslUnit -> [Issue]
analyzeTypeSizeIssues ast = 
    checkPointerToInt ast
    ++ checkPointerToUInt ast
    ++ checkIntToPointer ast
    ++ checkLongToPointer ast
    ++ checkSizeOfInt ast
    ++ checkSizeOfLong ast
    ++ checkIntAsSizet ast
    ++ checkIntAsPtrdifft ast
    ++ checkUIntAsMemSize ast

-- CastPointerToInt
checkPointerToInt :: CTranslUnit -> [Issue]
checkPointerToInt (CTranslUnit decls _) = 
    concatMap (analyzeDecl checkCast Map.empty) decls
  where
    checkCast :: VarContext -> CExpression NodeInfo -> [Issue]
    checkCast ctx (CCast targetDecl sourceExpr info) =
        if isIntType targetDecl && exprMightBePointerWithContext ctx sourceExpr
        then [createIssue undefined info Critical CastPointerToInt]
        else checkExprRecursive ctx sourceExpr
    checkCast ctx expr = checkExprRecursive ctx expr
    
    checkExprRecursive :: VarContext -> CExpression NodeInfo -> [Issue]
    checkExprRecursive ctx expr = case expr of
        CCast targetDecl sourceExpr info ->
            (if isIntType targetDecl && exprMightBePointerWithContext ctx sourceExpr
             then [createIssue undefined info Critical CastPointerToInt]
             else []) ++ checkExprRecursive ctx sourceExpr
        
        CBinary _ left right _ -> 
            checkExprRecursive ctx left ++ checkExprRecursive ctx right
        
        CUnary _ operand _ -> 
            checkExprRecursive ctx operand
        
        CAssign _ left right _ -> 
            checkExprRecursive ctx left ++ checkExprRecursive ctx right
        
        CCall fn args _ -> 
            checkExprRecursive ctx fn ++ concatMap (checkExprRecursive ctx) args
        
        _ -> []


-- CastPointerToUInt
checkPointerToUInt :: CTranslUnit -> [Issue]
checkPointerToUInt _ = []

-- CastIntToPointer
checkIntToPointer :: CTranslUnit -> [Issue]
checkIntToPointer _ = []

-- CastLongToPointer
checkLongToPointer :: CTranslUnit -> [Issue]
checkLongToPointer _ = []

-- SizeOfIntIsVoid
checkSizeOfInt :: CTranslUnit -> [Issue]
checkSizeOfInt _ = []

-- SizeOfLongIsVoid
checkSizeOfLong :: CTranslUnit -> [Issue]
checkSizeOfLong _ = []

-- UsingIntAsSizet
checkIntAsSizet :: CTranslUnit -> [Issue]
checkIntAsSizet _ = []

-- UsingIntAsPtrdifft
checkIntAsPtrdifft :: CTranslUnit -> [Issue]
checkIntAsPtrdifft _ = []

-- UsingUIntAsMemSize
checkUIntAsMemSize :: CTranslUnit -> [Issue]
checkUIntAsMemSize _ = []
