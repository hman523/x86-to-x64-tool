module Analysis.ASTTraversal where

import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)

import Analysis.TypeChecker (TypeEnv, collectDecl)
import Analysis.IssueTypes  (Issue)

-- ---------------------------------------------------------------------------
-- AST traversal helpers
--
-- The general pattern used by each analysis pass is:
--
--   concatMap (analyzeDecl myChecker initialEnv) (translUnitDecls ast)
--
-- where `myChecker :: TypeEnv -> CExpression NodeInfo -> [Issue]` inspects a
-- single expression and returns any issues it finds.  The traversal helpers
-- below handle the structural descent through declarations, function bodies,
-- statements, and expressions, threading a TypeEnv through as they go so that
-- checkers can query variable types at each point.
-- ---------------------------------------------------------------------------

-- | Entry point for a single top-level declaration.  Only function
--   definitions contain statements and expressions worth checking; bare
--   declarations (e.g. global variable declarations, typedefs, structs) are
--   skipped here and handled separately by the type-checker.
analyzeDecl :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CExternalDeclaration NodeInfo
            -> [Issue]
analyzeDecl f ctx (CFDefExt funDef) = analyzeFunDef f ctx funDef
analyzeDecl _ _ (CDeclExt _)        = []
analyzeDecl _ _ _                   = []

-- | Descend into a function definition, skipping the signature and going
--   straight to the body statement.
analyzeFunDef :: (TypeEnv -> CExpression NodeInfo -> [Issue])
              -> TypeEnv
              -> CFunctionDef NodeInfo
              -> [Issue]
analyzeFunDef f ctx (CFunDef _ _ _ stmt _) = analyzeStmt f ctx stmt

-- | Recursively walk a statement, collecting issues from every expression it
--   contains.  The TypeEnv is extended as new local variables come into scope
--   (compound blocks and for-loop initialisers), so that nested checkers see
--   the types declared above them.
analyzeStmt :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CStatement NodeInfo
            -> [Issue]
analyzeStmt f env stmt = case stmt of
    -- A bare expression-statement: walk the single expression.
    CExpr (Just expr) _    -> walkExpr f env expr

    -- A compound block { ... }: process each item left-to-right, extending
    -- the environment whenever a new declaration is encountered.
    CCompound _ items _    ->
        let (issues, _) = foldl (stepItem f) ([], env) items
        in issues

    -- if/else: check the condition and both branches.
    CIf cond thenS elseS _ ->
        walkExpr f env cond
        ++ analyzeStmt f env thenS
        ++ maybe [] (analyzeStmt f env) elseS

    -- while loop: check condition then body.
    CWhile cond body _ _   -> walkExpr f env cond ++ analyzeStmt f env body

    -- for loop: the init clause may introduce a new variable, so we extend
    -- the environment before checking the condition, step, and body.
    CFor forInit cond step body _ ->
        let initIssues = case forInit of
                Left (Just expr) -> walkExpr f env expr
                Left Nothing     -> []
                Right decl       -> analyzeDeclration f env decl
            env'       = case forInit of
                Right decl -> collectDecl decl env
                _          -> env
            condIssues = maybe [] (walkExpr f env') cond
            stepIssues = maybe [] (walkExpr f env') step
            bodyIssues = analyzeStmt f env' body
        in initIssues ++ condIssues ++ stepIssues ++ bodyIssues

    -- return statement: check the returned expression.
    CReturn (Just expr) _  -> walkExpr f env expr

    -- Other statements (break, continue, goto, labels, …) contain no
    -- sub-expressions that need checking.
    _                      -> []

-- | Apply the checker `f` to an expression and then recurse into all of its
--   direct sub-expressions, accumulating issues from the whole sub-tree.
--   `childExprs` defines which sub-expressions belong to each node kind.
walkExpr :: (TypeEnv -> CExpression NodeInfo -> [Issue])
         -> TypeEnv
         -> CExpression NodeInfo
         -> [Issue]
walkExpr f env expr = f env expr ++ concatMap (walkExpr f env) (childExprs expr)

-- | Return the direct child expressions of a C expression node.  Only
--   expression-valued sub-terms are included; type names and identifiers are
--   not expressions and are ignored.
childExprs :: CExpression NodeInfo -> [CExpression NodeInfo]
childExprs expr = case expr of
    CAssign _ l r _    -> [l, r]           -- lhs = rhs
    CBinary _ l r _    -> [l, r]           -- l op r
    CUnary  _ e _      -> [e]              -- op e  (or  e op)
    CCast   _ e _      -> [e]              -- (type) e
    CCall fn args _    -> fn : args        -- fn(arg1, arg2, ...)
    CMember e _ _ _    -> [e]              -- e.field  or  e->field
    CIndex  e1 e2 _    -> [e1, e2]         -- e1[e2]
    CCond c t e _      -> maybe [c, e] (\t' -> [c, t', e]) t  -- c ? t : e
    CComma  es _       -> es               -- e1, e2, ...
    _                  -> []               -- literals, variables, sizeof, etc.

-- | Process a single item inside a compound block, accumulating issues and
--   extending the type environment.  Declarations extend the environment for
--   all subsequent items in the same block; statements do not.
stepItem :: (TypeEnv -> CExpression NodeInfo -> [Issue])
         -> ([Issue], TypeEnv)
         -> CCompoundBlockItem NodeInfo
         -> ([Issue], TypeEnv)
stepItem f (issues, env) item = case item of
    CBlockDecl decl ->
        let env'   = collectDecl decl env          -- extend env for later items
            newIss = analyzeDeclration f env decl  -- check initialisers with pre-extension env
        in (issues ++ newIss, env')
    CBlockStmt stmt ->
        (issues ++ analyzeStmt f env stmt, env)
    _ -> (issues, env)

-- | Check a declaration's initialisers and bit-field expressions.  Each
--   declarator in a `CDecl` can have an optional initialiser or an optional
--   bit-field size expression; both are checked if present.
analyzeDeclration :: (TypeEnv -> CExpression NodeInfo -> [Issue])
                  -> TypeEnv
                  -> CDeclaration NodeInfo
                  -> [Issue]
analyzeDeclration f env (CDecl _ declrs _) = concatMap (analyzeDeclr f env) declrs
  where
    analyzeDeclr g ctx (_, maybeInit, maybeExpr) =
        case maybeExpr of
            Just expr -> g ctx expr   -- bit-field size expression
            Nothing   ->
                case maybeInit of
                    Just ini -> analyzeInit g ctx ini  -- variable initialiser
                    Nothing  -> []
analyzeDeclration _ _ (CStaticAssert _ _ _) = []

-- | Descend into an initialiser, which may itself be a nested initialiser
--   list (for aggregate types) or a single expression.
analyzeInit :: (TypeEnv -> CExpression NodeInfo -> [Issue])
            -> TypeEnv
            -> CInitializer NodeInfo
            -> [Issue]
analyzeInit f env (CInitExpr expr _)     = walkExpr f env expr
analyzeInit f env (CInitList initList _) =
    concatMap (\(_, ini) -> analyzeInit f env ini) initList
