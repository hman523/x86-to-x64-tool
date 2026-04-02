module Analysis.FunctionSignatures where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeFunctionSignatureIssues :: CTranslUnit -> [Issue]
analyzeFunctionSignatureIssues ast =
    checkFnsReturnPtrAsInt ast
    ++ checkFnsReturnPtrAsLong ast
    ++ checkFnsParamDeclaredAsIntTakesPtr ast
    ++ checkVaargUsingWrongTypesForPtrArgs ast

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build a TypeEnv populated with a function's formal parameters.
buildFuncParamEnv :: CFunctionDef NodeInfo -> TypeEnv
buildFuncParamEnv (CFunDef _ (CDeclr _ derived _ _ _) _ _ _) =
    foldr collectDecl Map.empty params
  where
    params = [d | CFunDeclr (Right (ds, _)) _ _ <- derived, d <- ds]

-- | Walk all return statements in @stmt@, applying @f@ to each returned
--   expression together with the NodeInfo of the @return@ keyword.
walkReturns :: (TypeEnv -> CExpression NodeInfo -> NodeInfo -> [Issue])
            -> TypeEnv
            -> CStatement NodeInfo
            -> [Issue]
walkReturns f env stmt = case stmt of
    CReturn (Just expr) info -> f env expr info
    CCompound _ items _      ->
        let (issues, _) = foldl (stepRet f) ([], env) items
        in issues
    CIf _ t e _              -> walkReturns f env t ++ maybe [] (walkReturns f env) e
    CWhile _ body _ _        -> walkReturns f env body
    CFor _ _ _ body _        -> walkReturns f env body
    _                        -> []

stepRet :: (TypeEnv -> CExpression NodeInfo -> NodeInfo -> [Issue])
        -> ([Issue], TypeEnv)
        -> CCompoundBlockItem NodeInfo
        -> ([Issue], TypeEnv)
stepRet f (issues, env) item = case item of
    CBlockDecl decl -> (issues, collectDecl decl env)
    CBlockStmt stmt -> (issues ++ walkReturns f env stmt, env)
    _               -> (issues, env)

-- ---------------------------------------------------------------------------
-- Checks
-- ---------------------------------------------------------------------------

-- | Flag functions declared to return @int@ that actually return a pointer.
checkFnsReturnPtrAsInt :: CTranslUnit -> [Issue]
checkFnsReturnPtrAsInt ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (checkTop tenv) decls
  where
    checkTop tenv (CFDefExt funDef@(CFunDef specs _ _ body _)) =
        let retType  = resolveTypedef tenv (resolveType specs [])
            paramEnv = buildFuncParamEnv funDef
        in if isIntType' retType
           then walkReturns (checkRetPtr tenv FnsReturnPtrAsInt) paramEnv body
           else []
    checkTop _ _ = []

    checkRetPtr tenv tag env expr info =
        [ createIssue info Critical tag
        | isPointer (resolveTypedef tenv (typeOfExpr env expr)) ]

-- | Flag functions declared to return @long@ that actually return a pointer.
checkFnsReturnPtrAsLong :: CTranslUnit -> [Issue]
checkFnsReturnPtrAsLong ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (checkTop tenv) decls
  where
    checkTop tenv (CFDefExt funDef@(CFunDef specs _ _ body _)) =
        let retType  = resolveTypedef tenv (resolveType specs [])
            paramEnv = buildFuncParamEnv funDef
        in if isLongType' retType
           then walkReturns (checkRetPtr tenv FnsReturnPtrAsLong) paramEnv body
           else []
    checkTop _ _ = []

    checkRetPtr tenv tag env expr info =
        [ createIssue info Critical tag
        | isPointer (resolveTypedef tenv (typeOfExpr env expr)) ]

-- | Flag assignments where a parameter declared as int receives a pointer value.
checkFnsParamDeclaredAsIntTakesPtr :: CTranslUnit -> [Issue]
checkFnsParamDeclaredAsIntTakesPtr ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (checkTop tenv) decls
  where
    checkTop tenv (CFDefExt funDef@(CFunDef _ _ _ body _)) =
        let paramEnv = buildFuncParamEnv funDef
        in analyzeStmt (checkAssign tenv paramEnv) paramEnv body
    checkTop _ _ = []

    checkAssign tenv paramEnv env (CAssign CAssignOp lhs rhs info) =
        case lhs of
            CVar (Ident name _ _) _ ->
                case Map.lookup name paramEnv of
                    Just (declaredType, mDeclPos)
                        | isIntType' (resolveTypedef tenv declaredType) ->
                            let rhsType = resolveTypedef tenv (typeOfExpr env rhs)
                            in [ createIssueWithDecl info mDeclPos Critical FnsParamDeclaredAsIntTakesPtr
                               | isPointer rhsType ]
                    _ -> []
            _ -> []
    checkAssign _ _ _ _ = []

-- | Flag @va_arg@ calls that extract @int@/@unsigned int@ (likely wrong for pointer args).
checkVaargUsingWrongTypesForPtrArgs :: CTranslUnit -> [Issue]
checkVaargUsingWrongTypesForPtrArgs ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkVaArg tenv) Map.empty) decls
  where
    checkVaArg tenv _env (CBuiltinExpr (CBuiltinVaArg _ decl info)) =
        let t = resolveTypedef tenv (typeOfDecl decl)
        in [ createIssue info Warning VaargUsingWrongTypesForPtrArgs
           | isIntType' t ]
    checkVaArg _ _ _ = []