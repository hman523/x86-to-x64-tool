-- | Rewrites standalone @(long)@ and @(long *)@ casts that were not already
--   handled by 'CastSync' (which only updates casts paired with a retyped
--   variable in the 'RetypeMap').
--
--   On LP64, @(long)@ is a 64-bit operation whereas it was 32-bit on ILP32.
--   Similarly, @(long *)@ dereferences read 8 bytes instead of 4.  Leaving
--   these casts unchanged silently changes program semantics.
--
--   This pass unconditionally replaces:
--     * @(long) expr@          → @(int32_t) expr@
--     * @(unsigned long) expr@ → @(uint32_t) expr@
--     * @(long *) expr@        → @(int32_t *) expr@
--
--   It runs /after/ 'syncCasts' so that variable-paired casts have already
--   been specialised (e.g. to @(intptr_t)@) and won't be double-rewritten
--   here. The remaining @(long)@ casts are standalone and must be replaced
--   with the fixed-width equivalent.
module Transformer.StandaloneCastFix
    ( fixStandaloneCasts
    ) where

import Data.Generics          (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)

import Transformer.Helpers         (typedefSpec, hasExactlyOneLong, hasUnsignedSpec,
                                    isTypeSpec)

-- | Replace every remaining @(long)@ / @(unsigned long)@ cast expression
--   (including pointer variants like @(long *)@) with the corresponding
--   @int32_t@ / @uint32_t@ based cast.  Also handles @(long){expr}@ compound
--   literals.
fixStandaloneCasts :: CTranslUnit -> CTranslUnit
fixStandaloneCasts = everywhere (mkT fixCast)
  where
    fixCast :: CExpression NodeInfo -> CExpression NodeInfo
    fixCast (CCast (CDecl castSpecs castDeclrs castNi) inner exprNi)
        | hasExactlyOneLong castSpecs =
            let newSpec     = typedefSpec (if hasUnsignedSpec castSpecs
                                           then "uint32_t"
                                           else "int32_t")
                newSpecs    = newSpec : filter (not . isTypeSpec) castSpecs
            in CCast (CDecl newSpecs castDeclrs castNi) inner exprNi
    -- Compound literal: (long){expr} → (int32_t){expr}
    fixCast (CCompoundLit (CDecl castSpecs castDeclrs castNi) initList exprNi)
        | hasExactlyOneLong castSpecs =
            let newSpec     = typedefSpec (if hasUnsignedSpec castSpecs
                                           then "uint32_t"
                                           else "int32_t")
                newSpecs    = newSpec : filter (not . isTypeSpec) castSpecs
            in CCompoundLit (CDecl newSpecs castDeclrs castNi) initList exprNi
    fixCast e = e
