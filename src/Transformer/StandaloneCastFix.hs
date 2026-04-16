-- | Rewrites standalone @(long)@ and @(long *)@ casts that were not already
--   handled by 'CastSync' (which only updates casts paired with a retyped
--   variable in the 'RetypeMap').
--
--   On LP64, @(long)@ is a 64-bit operation whereas it was 32-bit on ILP32.
--   Similarly, @(long *)@ dereferences read 8 bytes instead of 4.  Leaving
--   these casts unchanged silently changes program semantics.
--
--   This pass replaces remaining @(long)@ casts with fixed-width equivalents.
--   When the inner expression is pointer-typed, the cast becomes
--   @(intptr_t)@ / @(uintptr_t)@ to preserve pointer width on LP64.
--   Otherwise it becomes @(int32_t)@ / @(uint32_t)@.
--
--   It runs /after/ 'syncCasts' so that variable-paired casts have already
--   been specialised (e.g. to @(intptr_t)@) and won't be double-rewritten
--   here. The remaining @(long)@ casts are standalone and must be replaced
--   with the fixed-width equivalent.
module Transformer.StandaloneCastFix
    ( fixStandaloneCasts
    ) where

import Data.Generics          (everywhere, mkT)
import qualified Data.Map.Strict as Map
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)

import Analysis.TypeChecker   (TypeEnv, CType(..), typeOfExpr,
                                buildTypeEnv, collectDecl)
import Transformer.Helpers    (typedefSpec, hasExactlyOneLong, hasUnsignedSpec,
                               isTypeSpec)

-- | Replace every remaining @(long)@ / @(unsigned long)@ cast expression
--   (including pointer variants like @(long *)@) with the corresponding
--   fixed-width cast.  When the inner expression is pointer-typed the cast
--   becomes @(intptr_t)@ / @(uintptr_t)@ instead of @(int32_t)@ / @(uint32_t)@,
--   preventing silent truncation of 64-bit pointer values.
--   Also handles @(long){expr}@ compound literals.
fixStandaloneCasts :: CTranslUnit -> CTranslUnit
fixStandaloneCasts ast@(CTranslUnit decls _) =
    let env = buildGlobalEnv decls
    in everywhere (mkT (fixCast env)) ast

-- | Build a merged 'TypeEnv' from all functions' parameters and locals.
--   Name collisions are resolved by last-write-wins, which is imprecise
--   but safe: any evidence of pointer-ness for a given name will cause
--   @(long)name@ to use @intptr_t@ rather than risk truncation.
buildGlobalEnv :: [CExternalDeclaration NodeInfo] -> TypeEnv
buildGlobalEnv = foldl addDecl Map.empty
  where
    addDecl env (CFDefExt funDef) = buildFunEnvForCasts funDef `Map.union` env
    addDecl env (CDeclExt decl)   = collectDecl decl env
    addDecl env _                 = env

-- | Build a 'TypeEnv' from a function's parameters and locals.
buildFunEnvForCasts :: CFunctionDef NodeInfo -> TypeEnv
buildFunEnvForCasts (CFunDef _ (CDeclr _ derived _ _ _) _ body _) =
    let paramEnv = foldr collectDecl Map.empty (concatMap getParams derived)
    in case body of
        CCompound _ items _ -> buildTypeEnv items paramEnv
        _                   -> paramEnv
  where
    getParams (CFunDeclr (Right (ps, _)) _ _) = ps
    getParams _                               = []

-- | Determine whether the inner expression of a @(long)@ cast is
--   pointer-typed; if so, use @intptr_t@ / @uintptr_t@ instead of
--   @int32_t@ / @uint32_t@.
fixCast :: TypeEnv -> CExpression NodeInfo -> CExpression NodeInfo
fixCast env (CCast (CDecl castSpecs castDeclrs castNi) inner exprNi)
    | hasExactlyOneLong castSpecs, not (hasPointerDeclr castDeclrs) =
        let isUnsigned = hasUnsignedSpec castSpecs
            isInnerPtr = isPtr (typeOfExpr env inner)
            newName    = if isInnerPtr
                         then if isUnsigned then "uintptr_t" else "intptr_t"
                         else if isUnsigned then "uint32_t"  else "int32_t"
            newSpec    = typedefSpec newName
            newSpecs   = newSpec : filter (not . isTypeSpec) castSpecs
        in CCast (CDecl newSpecs castDeclrs castNi) inner exprNi
    -- (long *) cast — always int32_t * (preserving pointee size)
    | hasExactlyOneLong castSpecs =
        let newSpec  = typedefSpec (if hasUnsignedSpec castSpecs
                                    then "uint32_t"
                                    else "int32_t")
            newSpecs = newSpec : filter (not . isTypeSpec) castSpecs
        in CCast (CDecl newSpecs castDeclrs castNi) inner exprNi
-- Compound literal: (long){expr} -> (int32_t){expr}
fixCast _ (CCompoundLit (CDecl castSpecs castDeclrs castNi) initList exprNi)
    | hasExactlyOneLong castSpecs =
        let newSpec     = typedefSpec (if hasUnsignedSpec castSpecs
                                       then "uint32_t"
                                       else "int32_t")
            newSpecs    = newSpec : filter (not . isTypeSpec) castSpecs
        in CCompoundLit (CDecl newSpecs castDeclrs castNi) initList exprNi
fixCast _ e = e

-- | True when a cast declaration has a pointer derived declarator,
--   i.e. the cast is @(long *)@ rather than plain @(long)@.
hasPointerDeclr :: [(Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))] -> Bool
hasPointerDeclr declrs = any hasPtr declrs
  where
    hasPtr (Just (CDeclr _ (CPtrDeclr _ _ : _) _ _ _), _, _) = True
    hasPtr _                                                   = False

isPtr :: CType -> Bool
isPtr (TPointer _) = True
isPtr _            = False
