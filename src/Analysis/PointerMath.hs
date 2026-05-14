module Analysis.PointerMath
  ( analyzePointerMathIssues
  , checkPtrDiffStoredAs32bit
  , checkPtrAddOverflow
  , checkPtrSubUnderflow
  , checkArrayIndexingIntInArrayOver2tothe31size
  ) where

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
--   Covers both assignment form (@diff = p1 - p2@) and initializer form
--   (@int diff = p1 - p2@).
checkPtrDiffStoredAs32bit :: CTranslUnit -> [Issue]
checkPtrDiffStoredAs32bit ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
    ++ concatMap (checkFunBody tenv) decls
  where
    -- Assignment form: diff = p1 - p2
    checkAssign tenv env (CAssign CAssignOp lhs rhs info) =
        let lhsType  = resolveTypedef tenv (typeOfExpr env lhs)
            mDeclPos = case lhs of
                CVar (Ident name _ _) _ -> lookupDeclPos env name
                _                       -> Nothing
        in case rhs of
            CBinary CSubOp l r _ ->
                let lt = resolveTypedef tenv (typeOfExpr env l)
                    rt = resolveTypedef tenv (typeOfExpr env r)
                in [ createIssueWithDecl info mDeclPos Critical PtrDiffStoredAs32bit
                   | (isIntType' lhsType || isUIntType lhsType) && isPointer lt && isPointer rt ]
            _ -> []
    checkAssign _ _ _ = []

    -- Initializer form: int diff = p1 - p2
    checkFunBody tenv (CFDefExt (CFunDef _ _ _ body _)) =
        checkStmt tenv Map.empty body
    checkFunBody _ _ = []

    checkStmt tenv env stmt = case stmt of
        CCompound _ items _ ->
            let (issues, _) = foldl (stepDecl tenv) ([], env) items
            in issues
        CIf _ t e _ -> checkStmt tenv env t ++ maybe [] (checkStmt tenv env) e
        CWhile _ body _ _ -> checkStmt tenv env body
        CFor forInit _ _ body _ ->
            let env' = case forInit of { Right d -> collectDecl d env; _ -> env }
            in checkStmt tenv env' body
        _ -> []

    stepDecl tenv (issues, env) item = case item of
        CBlockDecl decl ->
            let env'   = collectDecl decl env
                newIss = checkDeclInit tenv env decl
            in (issues ++ newIss, env')
        CBlockStmt stmt ->
            (issues ++ checkStmt tenv env stmt, env)
        _ -> (issues, env)

    checkDeclInit tenv env (CDecl specs declrs info) =
        let varType = resolveTypedef tenv (resolveType specs [])
        in if isIntType' varType || isUIntType varType
           then concatMap (checkDeclrInit tenv env info) declrs
           else []
    checkDeclInit _ _ (CStaticAssert {}) = []

    checkDeclrInit tenv env info (_, Just (CInitExpr rhs _), _) =
        case rhs of
            CBinary CSubOp l r _ ->
                let lt = resolveTypedef tenv (typeOfExpr env l)
                    rt = resolveTypedef tenv (typeOfExpr env r)
                in [ createIssue info Critical PtrDiffStoredAs32bit
                   | isPointer lt && isPointer rt ]
            _ -> []
    checkDeclrInit _ _ _ _ = []

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
           | (isPointer lt && (isIntType' rt || isUIntType rt))
             || (isPointer rt && (isIntType' lt || isUIntType lt)) ]
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
           | isIntType' idxType || isUIntType idxType ]
    checkExpr _ _ _ = []
