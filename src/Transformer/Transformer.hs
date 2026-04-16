-- | Public API for the transformation pass.
--
--   'transform' produces a complete, semantically equivalent 64-bit rewrite
--   of a 32-bit C translation unit.  It composes the following passes:
--
--   1. Linter pipeline (filtered to SE-only fixes):
--        * Variable declaration retypings (ptrdiff_t, size_t, off_t, intptr_t)
--        * Function parameter retypings and va_arg type corrections
--        * Format-string specifier fixes for already-typed size_t/ptrdiff_t/ptr
--
--   2. 'transformStructMembers': @long@ struct/union member → @int32_t@
--
--   3. 'transformTypedefs': @typedef long T@ → @typedef int32_t T@
--
--   4. 'transformSizeofLong': @sizeof(long)@ → @sizeof(int32_t)@
--
--   5. 'transformReturnTypes': @long f(...)@ return type → classified type
--
--   6. 'transformLongs': remaining @long@ variable declarations → fixed-width
--      type via usage-based classification; produces a 'RetypeMap'.
--
--   7. 'syncCasts': @(long)@ casts paired with a retyped variable →
--      matching fixed-width cast.
--
--   8. 'fixStandaloneCasts': remaining standalone @(long)@ / @(long *)@
--      casts not paired with a retyped variable → @(int32_t)@ / @(int32_t *)@.
--
--   Non-SE linter fixes excluded (see 'nonSETypes'):
--     cast-only rewrites, return-expression wrapping, %ld/%lu specifier fixes.
--
--   The transformer is intentionally separate from both the analyser and the
--   linter as user-facing tools: run '-t' or '-l', never both.
module Transformer.Transformer
    ( transform
    , addRequiredIncludes
    ) where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants  (CInteger(..), CIntFlag(..), clearFlag, testFlag)
import Language.C.Data.Node         (NodeInfo)
import Language.C.Data.Ident        (Ident(..))
import Data.Generics                (everywhere, mkT)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Analysis.Analysis                       (analysis)
import Analysis.IssueTypes                     (IssueTag(..), issueType)
import Linter.Linter                           (lint)
import Transformer.LongReplacement             (RetypeMap, transformLongs,
                                                transformSizeofLong,
                                                splitMultiLongDecls)
import Transformer.StructMemberReplacement     (transformStructMembers)
import Transformer.TypedefReplacement          (transformTypedefs)
import Transformer.ReturnTypeReplacement       (transformReturnTypes)
import Transformer.CastSync                    (syncCasts)
import Transformer.StandaloneCastFix            (fixStandaloneCasts)
import Transformer.FunPtrReplacement            (transformFunPtrParams)
import Transformer.FormatFix                   (fixFormatStrings)

-- | Issue types whose linter fixes are NOT semantically equivalent.
nonSETypes :: [IssueTag]
nonSETypes =
    [ CastPointerToInt
    , CastPointerToUInt
    , CastIntToPointer
    , CastLongToPointer
    , HandleTypesCastToInt
    , FnsReturnPtrAsInt
    , FnsReturnPtrAsLong
    , LdUsedWithLongAssuming64bits
    , LuUsedForPtrSizedVals
    ]

-- | Produce a semantically equivalent 64-bit version of the translation unit.
transform :: CTranslUnit -> CTranslUnit
transform ast =
    let issues        = filter ((`notElem` nonSETypes) . issueType) (analysis ast)
        (ast1, _)     = lint ast issues
        ast2          = transformStructMembers ast1
        ast3          = transformTypedefs ast2
        ast4          = transformSizeofLong ast3
        ast5          = transformReturnTypes ast4
        ast5a         = splitMultiLongDecls ast5
        (ast6, rmap)  = transformLongs ast5a
        ast7          = transformFunPtrParams ast6
        ast8          = syncCasts rmap ast7
        ast9          = fixStandaloneCasts ast8
        ast10         = fixFormatStrings rmap ast9
        ast11         = stripLongSuffixes rmap ast10
    in ast11

-- | Prepend @#include@ directives required by types introduced during
--   transformation.  Operates on the already pretty-printed source string
--   because the parser strips preprocessor directives from the AST.
addRequiredIncludes :: String -> String
addRequiredIncludes src =
    let stdint  = any (`isInfixOf` src) ["int32_t", "uint32_t", "intptr_t", "uintptr_t"]
        stddef  = any (`isInfixOf` src) ["size_t", "ptrdiff_t"]
        headers = (if stdint then ["#include <stdint.h>"] else [])
               ++ (if stddef then ["#include <stddef.h>"] else [])
    in if null headers then src
       else unlines headers ++ "\n" ++ src

-- ---------------------------------------------------------------------------
-- L-suffix stripping
-- ---------------------------------------------------------------------------

-- | Strip @L@ / @UL@ suffixes from integer literals whose variable has been
--   retyped to a 32-bit type (@int32_t@ / @uint32_t@).  After
--   @long x = 100L@ becomes @int32_t x = 100L@, the @L@ suffix means the
--   literal is promoted to @long@ (64-bit on LP64), which triggers
--   @-Wimplicit-conversion@ warnings and is misleading.
--
--   Only strips the 'FlagLong' flag; 'FlagUnsigned' is preserved so that
--   @100UL@ for a @uint32_t@ variable becomes @100U@ rather than plain @100@.
stripLongSuffixes :: RetypeMap -> CTranslUnit -> CTranslUnit
stripLongSuffixes rmap = everywhere (mkT fixDecl) . everywhere (mkT fixAssign)
  where
    -- Check if the replacement type is a 32-bit typedef (int32_t or uint32_t)
    is32bit name = case Map.lookup name rmap of
        Just spec -> typedefNameOf spec `elem` [Just "int32_t", Just "uint32_t"]
        Nothing   -> False

    typedefNameOf (CTypeSpec (CTypeDef (Ident n _ _) _)) = Just n
    typedefNameOf _                                       = Nothing

    -- Strip L from integer constants in declaration initialisers
    fixDecl :: CDeclaration NodeInfo -> CDeclaration NodeInfo
    fixDecl (CDecl specs declrs ni) = CDecl specs (map fixDeclr declrs) ni
    fixDecl d = d

    fixDeclr (declr@(Just (CDeclr (Just (Ident n _ _)) _ _ _ _)),
              Just (CInitExpr initExpr iNi), bExpr)
        | is32bit n
        = (declr, Just (CInitExpr (stripLong initExpr) iNi), bExpr)
    fixDeclr d = d

    -- Strip L from integer constants in assignment RHS
    fixAssign :: CExpression NodeInfo -> CExpression NodeInfo
    fixAssign (CAssign op lhs@(CVar (Ident n _ _) _) rhs ni)
        | is32bit n = CAssign op lhs (stripLong rhs) ni
    fixAssign e = e

    -- Strip FlagLong from a CIntConst if present
    stripLong :: CExpression NodeInfo -> CExpression NodeInfo
    stripLong (CConst (CIntConst (CInteger val repr flags) ci))
        | testFlag FlagLong flags
        = CConst (CIntConst (CInteger val repr (clearFlag FlagLong flags)) ci)
    stripLong e = e
