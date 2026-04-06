module Analysis.PointerMath where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzePointerMathIssues :: CTranslUnit -> [Issue]
analyzePointerMathIssues ast =
    checkPtrDiffStoredAs32bit ast
    ++ checkPtrAddOverflow ast
    ++ checkPtrSubUnderflow ast
    ++ checkArrayIndexingIntInArrayOver2tothe31size ast

-- | Flag when pointer subtraction result is stored in int/uint (should be ptrdiff_t).
checkPtrDiffStoredAs32bit :: CTranslUnit -> [Issue]
checkPtrDiffStoredAs32bit ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CAssign CAssignOp lhs rhs info) =
        let lhsType  = resolveTypedef tenv (typeOfExpr env lhs)
            mDeclPos = case lhs of
                CVar (Ident name _ _) _ -> lookupDeclPos env name
                _                       -> Nothing
        in case rhs of
            CBinary CSubOp l r _ ->
                let lt = resolveTypedef tenv (typeOfExpr env l)
                    rt = resolveTypedef tenv (typeOfExpr env r)
                in [ createIssueWithDecl info mDeclPos Critical PtrDiffStoredAs32bit
                   | isIntType' lhsType && isPointer lt && isPointer rt ]
            _ -> []
    checkExpr _ _ _ = []

-- | Flag pointer addition with an int/uint offset (should use ptrdiff_t/size_t).
checkPtrAddOverflow :: CTranslUnit -> [Issue]
checkPtrAddOverflow ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CBinary CAddOp l r info) =
        let lt = resolveTypedef tenv (typeOfExpr env l)
            rt = resolveTypedef tenv (typeOfExpr env r)
        in [ createIssue info Warning PointerAddOverflow
           | (isPointer lt && isIntType' rt) || (isPointer rt && isIntType' lt) ]
    checkExpr _ _ _ = []

-- | Flag pointer subtraction with an unsigned int (can underflow).
checkPtrSubUnderflow :: CTranslUnit -> [Issue]
checkPtrSubUnderflow ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CBinary CSubOp l r info) =
        let lt = resolveTypedef tenv (typeOfExpr env l)
            rt = resolveTypedef tenv (typeOfExpr env r)
        in [ createIssue info Warning PtrSubUnderflow
           | isPointer lt && isUIntType rt ]
    checkExpr _ _ _ = []

-- | Flag array indexing with an int/uint index (should use size_t or ptrdiff_t).
checkArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> [Issue]
checkArrayIndexingIntInArrayOver2tothe31size ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CIndex _ idx info) =
        let idxType  = resolveTypedef tenv (typeOfExpr env idx)
            mDeclPos = case idx of
                CVar (Ident name _ _) _ -> lookupDeclPos env name
                _                       -> Nothing
        in [ createIssueWithDecl info mDeclPos Warning ArrayIndexingIntInArrayOver2tothe31size
           | isIntType' idxType ]
    checkExpr _ _ _ = []
