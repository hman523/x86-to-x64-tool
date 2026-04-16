-- | Synchronises @(long)@ and @(unsigned long)@ cast expressions so they
--   match the fixed-width type that 'Transformer.LongReplacement' assigned
--   to the receiving variable.
--
--   After 'transformLongs' rewrites @long x = (long)p@ to
--   @intptr_t x = (long)p@, the cast @(long)@ is inconsistent: the variable
--   now holds an @intptr_t@ value but the cast still says @long@.  While
--   this is not /wrong/ at the C level (the value representation is identical
--   on LP64 for these types), it is misleading and produces compiler warnings.
--   This pass fixes those casts so the code reads naturally.
--
--   Only cast expressions that are /directly paired/ with a variable in the
--   'RetypeMap' are updated:
--     * Assignment RHS: @x = (long) expr@ where @x@ was retyped.
--     * Declaration initialiser: @long x = (long) expr@, after the
--       declaration spec has already been updated by 'transformLongs'.
--
--   Standalone casts that are not matched to a retyped variable are left
--   untouched.
module Transformer.CastSync
    ( syncCasts
    ) where

import qualified Data.Map.Strict as Map

import Data.Generics          (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)
import Language.C.Data.Ident  (Ident(..))

import Transformer.Helpers    (hasExactlyOneLong, isTypeSpec)
import Transformer.LongReplacement (RetypeMap)

-- | Update @(long)@ / @(unsigned long)@ casts wherever the receiving
--   variable is in the 'RetypeMap'.
syncCasts :: RetypeMap -> CTranslUnit -> CTranslUnit
syncCasts rmap = everywhere (mkT fixExpr) . everywhere (mkT fixDecl)
  where
    -- -----------------------------------------------------------------------
    -- Fix assignment RHS: x = (long) e  ->  x = (newSpec) e
    -- -----------------------------------------------------------------------
    fixExpr :: CExpression NodeInfo -> CExpression NodeInfo
    fixExpr (CAssign op lhs@(CVar (Ident n _ _) _) rhs ni)
        | Just newSpec <- Map.lookup n rmap
        , Just rhs'   <- rewriteCastExpr newSpec rhs
        = CAssign op lhs rhs' ni
    fixExpr e = e

    -- -----------------------------------------------------------------------
    -- Fix declaration initialiser: T x = (long) e  ->  T x = (newSpec) e
    -- -----------------------------------------------------------------------
    fixDecl :: CDeclaration NodeInfo -> CDeclaration NodeInfo
    fixDecl (CDecl specs declrs ni) = CDecl specs (map fixDeclr declrs) ni
    fixDecl d = d

    fixDeclr (Just (CDeclr (Just ident@(Ident n _ _)) derived asmn attrs dNi), Just (CInitExpr initExpr iNi), bExpr)
        | Just newSpec <- Map.lookup n rmap
        , Just initExpr' <- rewriteCastExpr newSpec initExpr
        = (Just (CDeclr (Just ident) derived asmn attrs dNi), Just (CInitExpr initExpr' iNi), bExpr)
    fixDeclr d = d

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | If the expression is a @(long)@ or @(unsigned long)@ cast, replace its
--   type specifier with @newSpec@.  Otherwise return 'Nothing'.
rewriteCastExpr :: CDeclarationSpecifier NodeInfo
                -> CExpression NodeInfo
                -> Maybe (CExpression NodeInfo)
rewriteCastExpr newSpec (CCast (CDecl castSpecs castDeclrs castNi) inner exprNi)
    | hasExactlyOneLong castSpecs =
        let newCastSpecs = newSpec : filter (not . isTypeSpec) castSpecs
        in Just (CCast (CDecl newCastSpecs castDeclrs castNi) inner exprNi)
rewriteCastExpr _ _ = Nothing
