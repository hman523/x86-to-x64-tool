module Analysis.ASTTraversal where

import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)

import Analysis.TypeChecker (TypeEnv, collectDecl)
import Analysis.IssueTypes  (Issue)

-- ---------------------------------------------------------------------------
-- AST traversal helpers
-- ---------------------------------------------------------------------------

analyzeDecl :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CExternalDeclaration NodeInfo
            -> [Issue]
analyzeDecl f ctx (CFDefExt funDef) = analyzeFunDef f ctx funDef
analyzeDecl _ _ (CDeclExt _)        = []
analyzeDecl _ _ _                   = []

analyzeFunDef :: (TypeEnv -> CExpression NodeInfo -> [Issue])
              -> TypeEnv
              -> CFunctionDef NodeInfo
              -> [Issue]
analyzeFunDef f ctx (CFunDef _ _ _ stmt _) = analyzeStmt f ctx stmt

analyzeStmt :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CStatement NodeInfo
            -> [Issue]
analyzeStmt f env stmt = case stmt of
    CExpr (Just expr) _    -> walkExpr f env expr
    CCompound _ items _    ->
        let (issues, _) = foldl (stepItem f) ([], env) items
        in issues
    CIf cond thenS elseS _ ->
        walkExpr f env cond
        ++ analyzeStmt f env thenS
        ++ maybe [] (analyzeStmt f env) elseS
    CWhile cond body _ _   -> walkExpr f env cond ++ analyzeStmt f env body
    CFor init cond step body _ ->
        let initIssues = case init of
                Left (Just expr) -> walkExpr f env expr
                Left Nothing     -> []
                Right decl       -> analyzeDeclration f env decl
            env'       = case init of
                Right decl -> collectDecl decl env
                _          -> env
            condIssues = maybe [] (walkExpr f env') cond
            stepIssues = maybe [] (walkExpr f env') step
            bodyIssues = analyzeStmt f env' body
        in initIssues ++ condIssues ++ stepIssues ++ bodyIssues
    CReturn (Just expr) _  -> walkExpr f env expr
    _                      -> []

-- | Apply f to an expression and all of its sub-expressions recursively.
walkExpr :: (TypeEnv -> CExpression NodeInfo -> [Issue])
         -> TypeEnv
         -> CExpression NodeInfo
         -> [Issue]
walkExpr f env expr = f env expr ++ concatMap (walkExpr f env) (childExprs expr)

-- | Direct child expressions of a C expression node.
childExprs :: CExpression NodeInfo -> [CExpression NodeInfo]
childExprs expr = case expr of
    CAssign _ l r _    -> [l, r]
    CBinary _ l r _    -> [l, r]
    CUnary  _ e _      -> [e]
    CCast   _ e _      -> [e]
    CCall fn args _    -> fn : args
    CMember e _ _ _    -> [e]
    CIndex  e1 e2 _    -> [e1, e2]
    CCond c t e _      -> maybe [c, e] (\t' -> [c, t', e]) t
    CComma  es _       -> es
    _                  -> []

stepItem :: (TypeEnv -> CExpression NodeInfo -> [Issue])
         -> ([Issue], TypeEnv)
         -> CCompoundBlockItem NodeInfo
         -> ([Issue], TypeEnv)
stepItem f (issues, env) item = case item of
    CBlockDecl decl ->
        let env'   = collectDecl decl env
            newIss = analyzeDeclration f env decl
        in (issues ++ newIss, env')
    CBlockStmt stmt ->
        (issues ++ analyzeStmt f env stmt, env)
    _ -> (issues, env)

analyzeDeclration :: (TypeEnv -> CExpression NodeInfo -> [Issue])
                  -> TypeEnv
                  -> CDeclaration NodeInfo
                  -> [Issue]
analyzeDeclration f env (CDecl _ declrs _) = concatMap (analyzeDeclr f env) declrs
  where
    analyzeDeclr g ctx (_, maybeInit, maybeExpr) =
        case maybeExpr of
            Just expr -> g ctx expr
            Nothing   ->
                case maybeInit of
                    Just ini -> analyzeInit g ctx ini
                    Nothing  -> []

analyzeInit :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CInitializer NodeInfo
            -> [Issue]
analyzeInit f env (CInitExpr expr _)     = walkExpr f env expr
analyzeInit f env (CInitList initList _) =
    concatMap (\(_, ini) -> analyzeInit f env ini) initList
analyzeInit _ _ _                        = []
