module Analysis.Comparison
  ( analyzeComparisonIssues
  , checkLoopCounterAsIntWhenIteratingOverPtrArrays
  , checkPtrComparisonWithIntConsts
  , checkUsingIntForFileOffsets
  ) where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeComparisonIssues :: CTranslUnit -> [Issue]
analyzeComparisonIssues ast =
    checkLoopCounterAsIntWhenIteratingOverPtrArrays ast
    ++ checkPtrComparisonWithIntConsts ast
    ++ checkUsingIntForFileOffsets ast

-- | Flag for-loops where the counter is declared as int but the condition
--   involves a pointer subtraction (the counter should be ptrdiff_t).
checkLoopCounterAsIntWhenIteratingOverPtrArrays :: CTranslUnit -> [Issue]
checkLoopCounterAsIntWhenIteratingOverPtrArrays ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (checkTop tenv) decls
  where
    checkTop tenv (CFDefExt (CFunDef _ _ _ body _)) =
        walkStmt tenv Map.empty body
    checkTop _ _ = []

    walkStmt tenv env stmt = case stmt of
        CFor (Right decl) (Just cond) _ body info ->
            let env'       = collectDecl decl env
                ctrTypes   = getDeclTypes tenv decl
                condHasPD  = hasPtrDiff tenv env' cond
                mDeclPos   = firstDeclrNi decl
            in [ createIssueWithDecl info mDeclPos Warning LoopCounterAsIntWhenIteratingOverPtrArrays
               | any (\t -> isIntType' t || isUIntType t) ctrTypes && condHasPD ]
               ++ walkStmt tenv env' body
        CCompound _ items _ ->
            let (issues, _) = foldl (stepWalk tenv) ([], env) items
            in issues
        CIf _ t e _         -> walkStmt tenv env t ++ maybe [] (walkStmt tenv env) e
        CWhile cond body _ info ->
            (if hasPtrDiff tenv env cond && intBoundedByPtrDiff tenv env cond
             then [createIssue info Warning LoopCounterAsIntWhenIteratingOverPtrArrays]
             else [])
            ++ walkStmt tenv env body
        CFor _ _ _ body _   -> walkStmt tenv env body
        _                   -> []

    stepWalk tenv (issues, env) item = case item of
        CBlockDecl d -> (issues, collectDecl d env)
        CBlockStmt s -> (issues ++ walkStmt tenv env s, env)
        _            -> (issues, env)

    getDeclTypes tenv (CDecl specs declrs _) =
        [ resolveTypedef tenv (resolveType specs derived)
        | (Just (CDeclr _ derived _ _ _), _, _) <- declrs ]
    getDeclTypes _ (CStaticAssert {}) = []

    -- Extract the CDeclr NodeInfo from the first declarator in a CDecl
    firstDeclrNi (CDecl _ ((Just (CDeclr _ _ _ _ ni), _, _) : _) _) = Just ni
    firstDeclrNi _                                                    = Nothing

    -- True if an expression tree contains a pointer – pointer subtraction.
    hasPtrDiff tenv env expr = case expr of
        CBinary CSubOp l r _ ->
            let lt = resolveTypedef tenv (typeOfExpr env l)
                rt = resolveTypedef tenv (typeOfExpr env r)
            in (isPointer lt && isPointer rt)
               || hasPtrDiff tenv env l || hasPtrDiff tenv env r
        CBinary _ l r _ -> hasPtrDiff tenv env l || hasPtrDiff tenv env r
        _               -> False

    -- True when a comparison expression has an int/uint operand (indicating a
    -- loop variable bounded by the pointer difference on the other side).
    intBoundedByPtrDiff tenv env expr = case expr of
        CBinary op l r _ | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp] ->
            let lt = resolveTypedef tenv (typeOfExpr env l)
                rt = resolveTypedef tenv (typeOfExpr env r)
            in isIntType' lt || isUIntType lt || isIntType' rt || isUIntType rt
        CBinary CLndOp l r _ -> intBoundedByPtrDiff tenv env l || intBoundedByPtrDiff tenv env r
        CBinary CLorOp l r _ -> intBoundedByPtrDiff tenv env l || intBoundedByPtrDiff tenv env r
        _                    -> False

-- | Flag ordered comparisons between a pointer and an integer constant
--   (only eq/neq with 0 / NULL is valid).
checkPtrComparisonWithIntConsts :: CTranslUnit -> [Issue]
checkPtrComparisonWithIntConsts ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkExpr tenv) Map.empty) decls
  where
    checkExpr tenv env (CBinary op l r info)
        | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp] =
            let lt = resolveTypedef tenv (typeOfExpr env l)
                rt = resolveTypedef tenv (typeOfExpr env r)
            in case (isPointer lt, r) of
                (True, CConst (CIntConst _ _)) ->
                    [createIssue info Warning PtrComparisonWithIntConsts]
                _ -> case (isPointer rt, l) of
                    (True, CConst (CIntConst _ _)) ->
                        [createIssue info Warning PtrComparisonWithIntConsts]
                    _ -> []
        | op `elem` [CEqOp, CNeqOp] =
            let lt = resolveTypedef tenv (typeOfExpr env l)
                rt = resolveTypedef tenv (typeOfExpr env r)
            in case (isPointer lt, r) of
                (True, CConst (CIntConst n _)) | getCInteger n /= 0 ->
                    [createIssue info Warning PtrComparisonWithIntConsts]
                _ -> case (isPointer rt, l) of
                    (True, CConst (CIntConst n _)) | getCInteger n /= 0 ->
                        [createIssue info Warning PtrComparisonWithIntConsts]
                    _ -> []
    checkExpr _ _ _ = []

-- | Flag fseek/lseek calls where the offset argument is int/uint
--   (should be long or off_t).
checkUsingIntForFileOffsets :: CTranslUnit -> [Issue]
checkUsingIntForFileOffsets ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkCall tenv) Map.empty) decls
  where
    checkCall tenv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` seekFns, length args >= 2 =
            let offsetType = resolveTypedef tenv (typeOfExpr env (args !! 1))
                mDeclPos   = case args !! 1 of
                    CVar (Ident name _ _) _ -> lookupDeclPos env name
                    _                       -> Nothing
            in [ createIssueWithDecl info mDeclPos Warning UsingIntForFileOffsets
               | isIntType' offsetType || isUIntType offsetType ]
    checkCall _ _ _ = []

    seekFns :: [String]
    seekFns = ["fseek", "lseek", "fseeko", "lseeko64"]
