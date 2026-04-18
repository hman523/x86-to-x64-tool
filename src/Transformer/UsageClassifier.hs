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
import Language.C.Syntax.AST
import Language.C.Data.Node  (NodeInfo)
import Language.C.Data.Ident (Ident(..))

import Analysis.TypeChecker (TypeEnv, CType(..), typeOfExpr, buildTypeEnv, collectDecl)

import qualified Data.Map.Strict as Map

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

-- | Classify @name@ by scanning every expression and declaration initializer
--   in @funDef@ for usage evidence.  Falls back to 'NumberType' when no
--   evidence is found.
classifyVar :: TypeEnv -> String -> CFunctionDef NodeInfo -> AbstractType
classifyVar env name funDef =
    let exprEvidence = concatMap (evidenceFromExprNode env name)
                           (listify (const True) funDef :: [CExpression NodeInfo])
        declEvidence = concatMap (evidenceFromDeclNode env name)
                           (listify (const True) funDef :: [CDeclaration NodeInfo])
        evidence     = exprEvidence ++ declEvidence
    in if null evidence then NumberType
       else foldr1 stronger evidence

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
                       ++ if isBitwiseAssignOp op then [BitSeqType] else []

    -- Bitwise usage: x & e, x | e, x ^ e, x << e, x >> e
    CBinary op (CVar (Ident n _ _) _) _ _
        | n == name && isBitwiseOp op -> [BitSeqType]
    CBinary op _ (CVar (Ident n _ _) _) _
        | n == name && isBitwiseOp op -> [BitSeqType]

    -- Bitwise complement: ~x
    CUnary CCompOp (CVar (Ident n _ _) _) _
        | n == name -> [BitSeqType]

    _ -> []

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
        -> concatMap (rhsEvidenceFor self env) (maybe [] (:[]) thenE ++ [elseE])

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
classifyVarAcrossFuns :: String -> [CFunctionDef NodeInfo] -> AbstractType
classifyVarAcrossFuns name funDefs =
    let evidence = concatMap (classifyInFun name) funDefs
    in if null evidence then NumberType
       else foldr1 stronger evidence

-- | Classify @name@ within a single function by building its TypeEnv and
--   scanning for usage evidence.
classifyInFun :: String -> CFunctionDef NodeInfo -> [AbstractType]
classifyInFun name funDef@(CFunDef _ (CDeclr _ derived _ _ _) _ body _) =
    let paramEnv = foldr collectDecl Map.empty (concatMap getParams derived)
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
