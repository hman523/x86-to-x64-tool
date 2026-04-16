-- | Rewrites @long@ and @unsigned long@ variable declarations to
--   semantically equivalent fixed-width types using usage-based
--   classification from "Transformer.UsageClassifier".
--
--   Transformed locations:
--     * function parameters
--     * local variable declarations (including for-loop initialisers)
--     * file-scope (@extern@/@static@/plain) declarations
--     * @sizeof(long)@ / @sizeof(unsigned long)@ expressions
--
--   Deliberately excluded (require cross-function or cross-file data flow):
--     * struct / union member declarations  (handled by StructMemberReplacement)
--     * function return types               (handled by ReturnTypeReplacement)
--     * cast expressions                    (handled by CastSync)
module Transformer.LongReplacement
    ( RetypeMap
    , transformLongs
    , transformSizeofLong
    , splitMultiLongDecls
    , buildFunEnv
    , hasExactlyOneLong
    , hasUnsignedSpec
    , toSpec
    ) where

import qualified Data.Map.Strict as Map

import Data.Generics          (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)
import Language.C.Data.Ident  (Ident(..))

import Analysis.TypeChecker   (TypeEnv, buildTypeEnv, collectDecl)
import Transformer.Helpers    (typedefSpec, retypeDecl, hasExactlyOneLong,
                               hasUnsignedSpec, isTypeSpec)
import Transformer.UsageClassifier (AbstractType(..), classifyVar, classifyVarAcrossFuns)

-- | Maps each retyped variable name to its replacement
--   'CDeclarationSpecifier' (e.g. @typedefSpec "int32_t"@).
--   Produced by 'transformLongs' so that 'Transformer.CastSync' can update
--   any cast expressions that must match the new variable type.
type RetypeMap = Map.Map String (CDeclarationSpecifier NodeInfo)

-- ---------------------------------------------------------------------------
-- Top-level entry
-- ---------------------------------------------------------------------------

-- | Rewrite every @long@ / @unsigned long@ variable declaration in the
--   translation unit.  Returns the rewritten AST together with a 'RetypeMap'
--   recording every variable name that was retyped and the spec it received.
transformLongs :: CTranslUnit -> (CTranslUnit, RetypeMap)
transformLongs ast@(CTranslUnit decls _) =
    let funDefs = [fd | CFDefExt fd <- decls]
    in foldl (applyToDecl funDefs) (ast, Map.empty) decls

applyToDecl :: [CFunctionDef NodeInfo]
            -> (CTranslUnit, RetypeMap)
            -> CExternalDeclaration NodeInfo
            -> (CTranslUnit, RetypeMap)
applyToDecl _       (ast, rmap) (CFDefExt funDef) = applyToFunDef funDef ast rmap
applyToDecl funDefs (ast, rmap) (CDeclExt decl)   = applyToGlobalDecl funDefs decl ast rmap
applyToDecl _       pair _                         = pair

-- ---------------------------------------------------------------------------
-- Function definitions
-- ---------------------------------------------------------------------------

-- | Classify and retype @long@ variables local to a function.
applyToFunDef :: CFunctionDef NodeInfo
              -> CTranslUnit
              -> RetypeMap
              -> (CTranslUnit, RetypeMap)
applyToFunDef funDef ast rmap0 =
    let env  = buildFunEnv funDef
        vars = collectLongVarsInFun funDef
    in foldl (\(a, rm) (name, ni, isU, isCompBase) ->
            let cls     = if isCompBase
                          then NumberType   -- pointer/array-of-long: preserve base type
                          else classifyVar env name funDef
                newSpec = toSpec isU cls
            in (retypeDecl ni newSpec a, Map.insert name newSpec rm)
        ) (ast, rmap0) vars

-- | Build a TypeEnv covering both the explicit parameters and the top-level
--   locals of a function body.  This is used by the classifier to resolve
--   the types of expressions (e.g., knowing that @p@ is @int *@ so that
--   @x = (long)p@ implies 'PointerType').
buildFunEnv :: CFunctionDef NodeInfo -> TypeEnv
buildFunEnv (CFunDef _ (CDeclr _ derived _ _ _) _ body _) =
    let paramEnv = foldr collectDecl Map.empty (concatMap getParams derived)
    in case body of
        CCompound _ items _ -> buildTypeEnv items paramEnv
        _                   -> paramEnv
  where
    getParams (CFunDeclr (Right (ps, _)) _ _) = ps
    getParams _                            = []

-- ---------------------------------------------------------------------------
-- Global declarations (no function body -> default to NumberType)
-- ---------------------------------------------------------------------------

applyToGlobalDecl :: [CFunctionDef NodeInfo]
                  -> CDeclaration NodeInfo
                  -> CTranslUnit
                  -> RetypeMap
                  -> (CTranslUnit, RetypeMap)
applyToGlobalDecl funDefs decl ast rmap0 =
    foldl (\(a, rm) (name, ni, isU, _) ->
               let cls     = classifyVarAcrossFuns name funDefs
                   newSpec = toSpec isU cls
               in (retypeDecl ni newSpec a, Map.insert name newSpec rm))
          (ast, rmap0)
          (extractLongVars decl)

-- ---------------------------------------------------------------------------
-- Long-variable collection (manual traversal to exclude struct members)
-- ---------------------------------------------------------------------------

-- | Collect (name, declaratorNodeInfo, isUnsigned) for all @long@ /
--   @unsigned long@ variables in a function, covering parameters, locals,
--   and for-loop-init declarators but NOT struct/union member declarations.
collectLongVarsInFun :: CFunctionDef NodeInfo -> [(String, NodeInfo, Bool, Bool)]
collectLongVarsInFun funDef@(CFunDef _ _ _ body _) =
    collectLongParams funDef
    ++ case body of
        CCompound _ items _ -> collectLongLocals items
        _                   -> []

collectLongParams :: CFunctionDef NodeInfo -> [(String, NodeInfo, Bool, Bool)]
collectLongParams (CFunDef _ (CDeclr _ derived _ _ _) _ _ _) =
    concatMap getParamLongs derived
  where
    getParamLongs (CFunDeclr (Right (ps, _)) _ _) = concatMap extractLongVars ps
    getParamLongs _                            = []

collectLongLocals :: [CCompoundBlockItem NodeInfo] -> [(String, NodeInfo, Bool, Bool)]
collectLongLocals = concatMap go
  where
    go (CBlockDecl decl) = extractLongVars decl
    go (CBlockStmt stmt) = collectLongInStmt stmt
    go _                 = []

collectLongInStmt :: CStatement NodeInfo -> [(String, NodeInfo, Bool, Bool)]
collectLongInStmt stmt = case stmt of
    CCompound _ items _          -> collectLongLocals items
    CIf _ t e _                  -> collectLongInStmt t
                                    ++ maybe [] collectLongInStmt e
    CWhile _ body _ _            -> collectLongInStmt body
    CFor (Right decl) _ _ body _ -> extractLongVars decl
                                    ++ collectLongInStmt body
    CFor (Left _) _ _ body _     -> collectLongInStmt body
    CSwitch _ body _             -> collectLongInStmt body
    CLabel _ body _ _            -> collectLongInStmt body
    CCase _ body _               -> collectLongInStmt body
    CDefault body _              -> collectLongInStmt body
    _                            -> []

-- | Extract (name, declaratorNodeInfo, isUnsigned, isCompoundBase) from a
--   CDeclaration whose base type is @long@ or @unsigned long@ (but NOT
--   @long long@).
--
--   'isCompoundBase' is True when the declarator is a pointer or array whose
--   base type is @long@ (e.g. @long *arr@, @long arr[]@).  In that case the
--   @long@ describes the pointee/element type, not the variable itself, and
--   usage-based classification is not appropriate: we always replace with
--   @int32_t@ / @uint32_t@.
extractLongVars :: CDeclaration NodeInfo -> [(String, NodeInfo, Bool, Bool)]
extractLongVars (CDecl specs declrs _)
    | hasExactlyOneLong specs =
        [ (name, ni, hasUnsignedSpec specs, isCompoundBase derived)
        | (Just (CDeclr (Just (Ident name _ _)) derived _ _ ni), _, _) <- declrs
        ]
extractLongVars _ = []

-- | True when the first derived declarator makes this a pointer or array
--   (meaning @long@ is the element/pointee type, not the variable's own type).
isCompoundBase :: [CDerivedDeclarator NodeInfo] -> Bool
isCompoundBase (CPtrDeclr _ _   : _) = True
isCompoundBase (CArrDeclr _ _ _ : _) = True
isCompoundBase _                      = False

-- ---------------------------------------------------------------------------
-- AbstractType -> replacement specifier
-- ---------------------------------------------------------------------------

-- | Map an abstract type category and its original signedness to the
--   appropriate fixed-width typedef name.
toSpec :: Bool         -- ^ True = original type was unsigned
       -> AbstractType
       -> CDeclarationSpecifier NodeInfo
toSpec True  PointerType = typedefSpec "uintptr_t"
toSpec False PointerType = typedefSpec "intptr_t"
toSpec _     OffsetType  = typedefSpec "ptrdiff_t"
toSpec _     SizeType    = typedefSpec "size_t"
toSpec _     BitSeqType  = typedefSpec "uint32_t"
toSpec True  NumberType  = typedefSpec "uint32_t"
toSpec False NumberType  = typedefSpec "int32_t"

-- ---------------------------------------------------------------------------
-- sizeof(long) rewriting
-- ---------------------------------------------------------------------------

-- | Replace @sizeof(long)@ with @sizeof(int32_t)@, @sizeof(unsigned long)@
--   with @sizeof(uint32_t)@, and likewise for @_Alignof(long)@ /
--   @_Alignof(unsigned long)@ throughout the translation unit.
--   Also handles compound variants like @sizeof(long[N])@ and
--   @_Alignas(long)@ alignment specifiers.
--
--   On LP64, @sizeof(long)@ evaluates to 8 instead of the original 4.  Any
--   buffer sizing or array-length calculation that uses @sizeof(long)@ as a
--   32-bit count will be wrong.  Replacing with @sizeof(int32_t)@ restores
--   the original value.  The same reasoning applies to @_Alignof(long)@,
--   which changes from 4 to 8 on LP64.
transformSizeofLong :: CTranslUnit -> CTranslUnit
transformSizeofLong = everywhere (mkT fixSizeof) . everywhere (mkT fixAlignas)
  where
    fixSizeof :: CExpression NodeInfo -> CExpression NodeInfo
    fixSizeof (CSizeofType (CDecl specs declrs ni) sni)
        | hasExactlyOneLong specs =
            let newSpec = typedefSpec (if hasUnsignedSpec specs then "uint32_t" else "int32_t")
            in CSizeofType (CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni) sni
    fixSizeof (CAlignofType (CDecl specs declrs ni) sni)
        | hasExactlyOneLong specs =
            let newSpec = typedefSpec (if hasUnsignedSpec specs then "uint32_t" else "int32_t")
            in CAlignofType (CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni) sni
    fixSizeof e = e

    -- | Rewrite @_Alignas(long)@ specifiers embedded in declarations.
    fixAlignas :: CDeclarationSpecifier NodeInfo -> CDeclarationSpecifier NodeInfo
    fixAlignas (CAlignSpec (CAlignAsType (CDecl specs declrs ni) sni))
        | hasExactlyOneLong specs =
            let newSpec = typedefSpec (if hasUnsignedSpec specs then "uint32_t" else "int32_t")
            in CAlignSpec (CAlignAsType (CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni) sni)
    fixAlignas s = s

-- ---------------------------------------------------------------------------
-- Multi-declarator splitting
-- ---------------------------------------------------------------------------

-- | Split multi-declarator @long@ declarations (e.g. @long a = x, b = y;@)
--   into individual declarations (@long a = x; long b = y;@).
--
--   This ensures that each variable can be independently classified and
--   retyped by 'transformLongs' without one classification overwriting
--   another (since 'retypeDecl' operates on the whole CDecl's spec list).
--
--   The transformation is applied inside function bodies and at file scope.
splitMultiLongDecls :: CTranslUnit -> CTranslUnit
splitMultiLongDecls (CTranslUnit decls ni) =
    CTranslUnit (concatMap splitExtDecl decls) ni
  where
    splitExtDecl :: CExternalDeclaration NodeInfo -> [CExternalDeclaration NodeInfo]
    splitExtDecl (CDeclExt decl) = map CDeclExt (splitDecl decl)
    splitExtDecl (CFDefExt (CFunDef specs declr oldDecls body fni)) =
        [CFDefExt (CFunDef specs declr oldDecls (splitBody body) fni)]
    splitExtDecl other = [other]

    splitBody :: CStatement NodeInfo -> CStatement NodeInfo
    splitBody (CCompound labels items cni) =
        CCompound labels (concatMap splitItem items) cni
    splitBody s = s

    splitItem :: CCompoundBlockItem NodeInfo -> [CCompoundBlockItem NodeInfo]
    splitItem (CBlockDecl decl) = map CBlockDecl (splitDecl decl)
    splitItem (CBlockStmt stmt) = [CBlockStmt (splitStmt stmt)]
    splitItem other             = [other]

    splitStmt :: CStatement NodeInfo -> CStatement NodeInfo
    splitStmt (CCompound labels items cni) =
        CCompound labels (concatMap splitItem items) cni
    splitStmt (CIf cond t mElse sni) =
        CIf cond (splitStmt t) (fmap splitStmt mElse) sni
    splitStmt (CWhile cond body isDoWhile sni) =
        CWhile cond (splitStmt body) isDoWhile sni
    splitStmt (CFor init' cond step body sni) =
        let init'' = case init' of
                Right decl -> case splitDecl decl of
                    [d] -> Right d
                    _   -> Right decl  -- for-init can only have one decl
                other -> other
        in CFor init'' cond step (splitStmt body) sni
    splitStmt (CSwitch cond body sni) = CSwitch cond (splitStmt body) sni
    splitStmt (CLabel lbl body attrs sni) = CLabel lbl (splitStmt body) attrs sni
    splitStmt (CCase expr body sni) = CCase expr (splitStmt body) sni
    splitStmt (CDefault body sni) = CDefault (splitStmt body) sni
    splitStmt s = s

    -- | Split a CDecl with multiple declarators into one CDecl per
    --   declarator, but only if the base type is @long@ / @unsigned long@.
    splitDecl :: CDeclaration NodeInfo -> [CDeclaration NodeInfo]
    splitDecl (CDecl specs declrs dni)
        | hasExactlyOneLong specs, length declrs > 1
        = [ CDecl specs [d] dni | d <- declrs ]
    splitDecl d = [d]
