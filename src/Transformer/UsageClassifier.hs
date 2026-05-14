-- | Usage-based classification of C variables into the advisor's five
--   abstract-type categories.  The classification drives which fixed-width
--   type a @long@ (or @unsigned long@) variable should become.
--
--   Priority (highest wins):
--     PointerType > SizeType > OffsetType > BitSeqType > NumberType
--
--   Uses 'Analysis.TypeChecker.typeOfExpr' on the *original* (pre-transform)
--   AST, so classifications reflect 32-bit source semantics.
module Transformer.UsageClassifier
    ( AbstractType(..)
    , classifyVar
    , classifyVarAcrossFuns
    , rhsEvidence
    , stronger
    ) where

import Data.Generics        (listify)
import Data.Maybe           (maybeToList)
import Language.C.Syntax.AST
import Language.C.Data.Node     (NodeInfo)
import Language.C.Data.Ident    (Ident(..))
import Language.C.Data.Position (posOf)

import Analysis.TypeChecker (TypeEnv, CType(..), typeOfExpr, buildTypeEnv, collectDecl)
import Analysis.KnownFunctions (sizeArgFunctions)

-- ---------------------------------------------------------------------------
-- Abstract type categories (advisor taxonomy)
-- ---------------------------------------------------------------------------

data AbstractType
    = NumberType   -- ^ plain arithmetic  -> int32_t / uint32_t
    | BitSeqType   -- ^ bit manipulation  -> uint32_t
    | OffsetType   -- ^ pointer difference -> ptrdiff_t
    | SizeType     -- ^ memory size        -> size_t
    | PointerType  -- ^ holds an address   -> intptr_t / uintptr_t
    deriving (Show, Eq, Ord)

priority :: AbstractType -> Int
priority PointerType = 4
priority SizeType    = 3
priority OffsetType  = 2
priority BitSeqType  = 1
priority NumberType  = 0

stronger :: AbstractType -> AbstractType -> AbstractType
stronger a b = if priority a >= priority b then a else b

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Classify a specific @long@ declaration (identified by @declNi@) by
--   collecting usage evidence only within the scope where that declaration
--   is live.  Inner-scope redeclarations of the same @name@ correctly shadow
--   the target so their evidence does not bleed into the outer variable.
--   Falls back to 'NumberType' when no evidence is found.
classifyVar :: TypeEnv -> NodeInfo -> String -> CFunctionDef NodeInfo -> AbstractType
classifyVar env declNi name funDef =
    let evidence = collectFunEvidence env name declNi funDef
    in if null evidence then NumberType
       else foldr1 stronger evidence

-- ---------------------------------------------------------------------------
-- Scope-aware traversal
-- ---------------------------------------------------------------------------

-- | Walk the function body collecting evidence for the declaration at
--   @targetNi@.  If that NodeInfo matches a parameter the variable is in
--   scope from the very start of the body; otherwise it becomes in scope
--   once its @CBlockDecl@ is encountered.
collectFunEvidence :: TypeEnv -> String -> NodeInfo -> CFunctionDef NodeInfo -> [AbstractType]
collectFunEvidence env name targetNi (CFunDef _ (CDeclr _ derived _ _ _) _ body _) =
    let params       = concatMap getParams derived
        startInScope = any (matchesTargetDecl targetNi) params
    in walkStmt env name targetNi startInScope body
  where
    getParams (CFunDeclr (Right (ps, _)) _ _) = ps
    getParams _                               = []

-- | Walk a statement, threading the in-scope flag.
walkStmt :: TypeEnv -> String -> NodeInfo -> Bool -> CStatement NodeInfo -> [AbstractType]
walkStmt env name targetNi inScope stmt = case stmt of
    CCompound _ items _ ->
        walkItems env name targetNi inScope items

    CExpr (Just e) _ ->
        if inScope then allExprEvidence env name e else []

    CIf cond thenS elseS _ ->
        (if inScope then allExprEvidence env name cond else [])
        ++ walkStmt env name targetNi inScope thenS
        ++ maybe [] (walkStmt env name targetNi inScope) elseS

    CWhile cond body _ _ ->
        (if inScope then allExprEvidence env name cond else [])
        ++ walkStmt env name targetNi inScope body

    -- CFor init can introduce a new declaration that shadows `name`.
    -- The resulting inScope' is local to the for-loop (cond/incr/body).
    CFor initOrDecl cond incr body _ ->
        let (initEv, inScope') = case initOrDecl of
              Left (Just e) ->
                  (if inScope then allExprEvidence env name e else [], inScope)
              Left Nothing ->
                  ([], inScope)
              Right decl
                | declaresName name decl ->
                    let ev = if matchesTargetDecl targetNi decl
                             then evidenceFromDeclNode env name decl
                             else []
                    in (ev, matchesTargetDecl targetNi decl)
                | otherwise -> ([], inScope)
        in initEv
           ++ maybe [] (\e -> if inScope' then allExprEvidence env name e else []) cond
           ++ maybe [] (\e -> if inScope' then allExprEvidence env name e else []) incr
           ++ walkStmt env name targetNi inScope' body

    CSwitch e body _ ->
        (if inScope then allExprEvidence env name e else [])
        ++ walkStmt env name targetNi inScope body

    CLabel _ s _ _ ->
        walkStmt env name targetNi inScope s

    CCase e s _ ->
        (if inScope then allExprEvidence env name e else [])
        ++ walkStmt env name targetNi inScope s

    CCases e1 e2 s _ ->
        ( if inScope
          then allExprEvidence env name e1 ++ allExprEvidence env name e2
          else [] )
        ++ walkStmt env name targetNi inScope s

    CDefault s _ ->
        walkStmt env name targetNi inScope s

    CReturn (Just e) _ ->
        if inScope then allExprEvidence env name e else []

    CGotoPtr e _ ->
        if inScope then allExprEvidence env name e else []

    _ -> []

-- | Walk compound block items, flipping the in-scope flag whenever a
--   declaration for @name@ is encountered: True if it is our target,
--   False if it is a shadowing declaration.
walkItems :: TypeEnv -> String -> NodeInfo -> Bool -> [CCompoundBlockItem NodeInfo] -> [AbstractType]
walkItems _   _    _        _       [] = []
walkItems env name targetNi inScope (item:rest) = case item of
    CBlockDecl decl
        | declaresName name decl ->
            let ev       = if matchesTargetDecl targetNi decl
                           then evidenceFromDeclNode env name decl
                           else []
                inScope' = matchesTargetDecl targetNi decl
            in ev ++ walkItems env name targetNi inScope' rest
        | otherwise ->
            -- This declaration doesn't declare `name`, but its initializer
            -- expressions may contain usage patterns for `name` (e.g. y = x & 0xFF).
            (if inScope then declExprEvidence env name decl else [])
            ++ walkItems env name targetNi inScope rest
    CBlockStmt stmt ->
        walkStmt env name targetNi inScope stmt
        ++ walkItems env name targetNi inScope rest
    CNestedFunDef _ ->
        walkItems env name targetNi inScope rest

-- | Apply 'evidenceFromExprNode' to every sub-expression of @expr@,
--   preserving the listify-based unnesting the evidence functions expect.
allExprEvidence :: TypeEnv -> String -> CExpression NodeInfo -> [AbstractType]
allExprEvidence env name expr =
    concatMap (evidenceFromExprNode env name)
              (listify (const True) expr :: [CExpression NodeInfo])

-- | Collect evidence from all expressions embedded in a CDeclaration that
--   does not itself declare @name@: covers initializer expressions, array
--   size expressions, etc.
declExprEvidence :: TypeEnv -> String -> CDeclaration NodeInfo -> [AbstractType]
declExprEvidence env name decl =
    concatMap (evidenceFromExprNode env name)
              (listify (const True) decl :: [CExpression NodeInfo])

-- | True when @decl@ contains a declarator whose source position matches
--   @targetNi@.
matchesTargetDecl :: NodeInfo -> CDeclaration NodeInfo -> Bool
matchesTargetDecl targetNi (CDecl _ declrs _) =
    any match declrs
  where
    match (Just (CDeclr _ _ _ _ dNi), _, _) = posOf dNi == posOf targetNi
    match _                                  = False
matchesTargetDecl _ _ = False

-- | True when @decl@ has at least one declarator named @name@.
declaresName :: String -> CDeclaration NodeInfo -> Bool
declaresName name (CDecl _ declrs _) =
    any match declrs
  where
    match (Just (CDeclr (Just (Ident n _ _)) _ _ _ _), _, _) = n == name
    match _                                                   = False
declaresName _ _ = False

-- ---------------------------------------------------------------------------
-- Expression-level evidence
-- ---------------------------------------------------------------------------

-- | Check a single expression node (already unnested by 'listify') for
--   evidence about how @name@ is used.
evidenceFromExprNode :: TypeEnv -> String -> CExpression NodeInfo -> [AbstractType]
evidenceFromExprNode env name expr = case expr of
    -- Assignment: x = rhs -> evidence from what flows into x.
    -- Use rhsEvidenceFor so that sizeof(x) anywhere in rhs is suppressed.
    CAssign op (CVar (Ident n _ _) _) rhs _
        | n == name -> rhsEvidenceFor n env rhs
                       ++ [BitSeqType | isBitwiseAssignOp op]

    -- Bitwise usage: x & e, x | e, x ^ e, x << e, x >> e
    CBinary op (CVar (Ident n _ _) _) _ _
        | n == name && isBitwiseOp op -> [BitSeqType]
    CBinary op _ (CVar (Ident n _ _) _) _
        | n == name && isBitwiseOp op -> [BitSeqType]

    -- Bitwise complement: ~x
    CUnary CCompOp (CVar (Ident n _ _) _) _
        | n == name -> [BitSeqType]

    -- Function-call argument: f(..., x, ...) where f is a known
    -- size-consuming function (malloc, calloc, realloc, memcpy, memmove,
    -- memset, alloca) -> the variable is used as a byte count.
    CCall (CVar (Ident fname _ _) _) args _
        | fname `elem` sizeArgFunctions
        , any (isVarRef name) args -> [SizeType]

    _ -> []

-- | True when the expression is a direct reference to the given variable.
isVarRef :: String -> CExpression NodeInfo -> Bool
isVarRef name (CVar (Ident n _ _) _) = n == name
isVarRef _    _                       = False

-- ---------------------------------------------------------------------------
-- Declaration-initializer evidence
-- ---------------------------------------------------------------------------

-- | Check a CDeclaration node for an initializer for @name@, then report
--   evidence from that initializer expression.
evidenceFromDeclNode :: TypeEnv -> String -> CDeclaration NodeInfo -> [AbstractType]
evidenceFromDeclNode env name (CDecl _ declrs _) =
    [ ev
    | (Just (CDeclr (Just (Ident n _ _)) _ _ _ _), Just (CInitExpr initExpr _), _) <- declrs
    , n == name
    , ev <- rhsEvidenceFor n env initExpr
    ]
evidenceFromDeclNode _ _ _ = []

-- ---------------------------------------------------------------------------
-- RHS / initializer evidence
-- ---------------------------------------------------------------------------

-- | What abstract type does this expression imply when assigned to a @long@
--   variable named @self@?  Any @sizeof(self)@ encountered at any depth is
--   suppressed: the sizeof value changes when the variable is retyped, so it
--   must not be treated as 'SizeType' evidence.
--
--   This handles all the ways sizeof(x) can nest:
--     * direct:      @x = sizeof(x)@
--     * through cast: @x = (long)sizeof(x)@
--     * through ternary: @x = flag ? sizeof(x) : 0@
--     * in initializer: @long x = sizeof(x);@
rhsEvidenceFor :: String -> TypeEnv -> CExpression NodeInfo -> [AbstractType]
rhsEvidenceFor self env rhs = case rhs of
    -- sizeof(self) and _Alignof(self) are self-referential: their value
    -- changes when the variable is retyped, so suppress them entirely.
    -- sizeof/alignof of *other* expressions or types are legitimate evidence.
    --
    -- _Alignof is the only other type-introspective operator in C whose
    -- compile-time value depends on the declared type of its operand (just
    -- like sizeof).  All arithmetic/bitwise/comparison operators depend only
    -- on the runtime value, not the declared type, so they cannot cause the
    -- same circular-evidence problem.
    CSizeofExpr  (CVar (Ident n _ _) _) _ | n == self -> []
    CAlignofExpr (CVar (Ident n _ _) _) _ | n == self -> []
    -- sizeof(other expr) or sizeof(type) -> variable stores a byte count
    CSizeofExpr _ _  -> [SizeType]
    CSizeofType _ _  -> [SizeType]
    -- _Alignof does not generate SizeType (or any) evidence: alignment values
    -- are too small and context-specific to reliably indicate a size_t use.

    -- ptr - ptr -> the variable stores a pointer difference
    CBinary CSubOp l r _
        | isPtr (typeOfExpr env l) && isPtr (typeOfExpr env r) -> [OffsetType]

    -- cast from pointer expression -> the variable holds an address;
    -- otherwise fall through to the inner expression (e.g. a cast wrapping
    -- a ptr-diff or a sizeof still carries that evidence)
    CCast _ inner _
        | isPtr (typeOfExpr env inner) -> [PointerType]
        | otherwise                    -> rhsEvidenceFor self env inner

    -- ternary: classify both branches, take the stronger evidence
    CCond _ thenE elseE _
        -> concatMap (rhsEvidenceFor self env) (maybeToList thenE ++ [elseE])

    -- any other pointer-typed expression -> holds an address
    _ | isPtr (typeOfExpr env rhs) -> [PointerType]

    -- bitwise expression on the RHS -> bit sequence
    CBinary op _ _ _
        | isBitwiseOp op -> [BitSeqType]
    CUnary CCompOp _ _ -> [BitSeqType]

    -- no recognisable evidence
    _ -> []

-- | Exported wrapper: classify an RHS expression with no variable name in
--   scope (used by 'ReturnTypeReplacement' and tests).
rhsEvidence :: TypeEnv -> CExpression NodeInfo -> [AbstractType]
rhsEvidence = rhsEvidenceFor ""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isPtr :: CType -> Bool
isPtr (TPointer _) = True
isPtr _            = False

isBitwiseOp :: CBinaryOp -> Bool
isBitwiseOp op = op `elem` [CAndOp, COrOp, CXorOp, CShlOp, CShrOp]

isBitwiseAssignOp :: CAssignOp -> Bool
isBitwiseAssignOp op = op `elem` [CAndAssOp, COrAssOp, CXorAssOp, CShlAssOp, CShrAssOp]

-- ---------------------------------------------------------------------------
-- Cross-function classification for global variables
-- ---------------------------------------------------------------------------

-- | Classify a global variable by scanning all function bodies in the
--   translation unit.  Each function contributes evidence; the highest-
--   priority category across all functions wins.  Falls back to 'NumberType'.
--   @globalEnv@ seeds each per-function TypeEnv so that function-call return
--   types are resolved correctly by 'typeOfExpr'.
classifyVarAcrossFuns :: TypeEnv -> String -> [CFunctionDef NodeInfo] -> AbstractType
classifyVarAcrossFuns globalEnv name funDefs =
    let evidence = concatMap (classifyInFun globalEnv name) funDefs
    in if null evidence then NumberType
       else foldr1 stronger evidence

-- | Classify @name@ within a single function by building its TypeEnv
--   (seeded with @globalEnv@) and scanning for usage evidence.
classifyInFun :: TypeEnv -> String -> CFunctionDef NodeInfo -> [AbstractType]
classifyInFun globalEnv name funDef@(CFunDef _ (CDeclr _ derived _ _ _) _ body _) =
    let paramEnv = foldr collectDecl globalEnv (concatMap getParams derived)
        env      = case body of
                     CCompound _ items _ -> buildTypeEnv items paramEnv
                     _                   -> paramEnv
        exprEvidence = concatMap (evidenceFromExprNode env name)
                           (listify (const True) funDef :: [CExpression NodeInfo])
        declEvidence = concatMap (evidenceFromDeclNode env name)
                           (listify (const True) funDef :: [CDeclaration NodeInfo])
    in exprEvidence ++ declEvidence
  where
    getParams (CFunDeclr (Right (ps, _)) _ _) = ps
    getParams _                            = []
