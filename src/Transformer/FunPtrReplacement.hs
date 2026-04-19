-- | Rewrites @long@ and @unsigned long@ types appearing in function-pointer
--   parameter declarations.
--
--   For a declaration like @void (*fp)(long n)@, the @long@ in the parameter
--   list is a 'CDeclaration' nested inside a 'CFunDeclr' derived declarator.
--   The main 'LongReplacement' pass only retypes top-level function params
--   and local variables; it does not descend into derived declarators.
--
--   This pass replaces @long@ with @int32_t@ / @uint32_t@ for every
--   parameter declaration inside every function-pointer declarator in the
--   translation unit.
module Transformer.FunPtrReplacement
    ( transformFunPtrParams
    ) where

import Data.Generics          (everywhere, mkT)
import Data.Char              (toLower)
import Data.List              (isInfixOf)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)
import Language.C.Data.Ident  (Ident(..))

import Transformer.Helpers         (typedefSpec, hasExactlyOneLong, hasUnsignedSpec,
                                    isTypeSpec)

-- | Replace @long@ / @unsigned long@ in function-pointer parameter
--   declarations throughout the translation unit.
transformFunPtrParams :: CTranslUnit -> CTranslUnit
transformFunPtrParams = everywhere (mkT fixDerivedDeclr)
  where
    -- Rewrite parameter declarations inside function-pointer declarators
    fixDerivedDeclr :: CDerivedDeclarator NodeInfo -> CDerivedDeclarator NodeInfo
    fixDerivedDeclr (CFunDeclr (Right (params, isVariadic)) attrs ni) =
        CFunDeclr (Right (map fixParam params, isVariadic)) attrs ni
    fixDerivedDeclr d = d

    fixParam :: CDeclaration NodeInfo -> CDeclaration NodeInfo
    fixParam (CDecl specs declrs ni)
        | hasExactlyOneLong specs =
            let isUnsigned = hasUnsignedSpec specs
                usePtr     = any paramLooksLikePtr declrs
                newSpec    = typedefSpec $ case (usePtr, isUnsigned) of
                    (True,  True)  -> "uintptr_t"
                    (True,  False) -> "intptr_t"
                    (False, True)  -> "uint32_t"
                    (False, False) -> "int32_t"
                newSpecs = newSpec : filter (not . isTypeSpec) specs
            in CDecl newSpecs declrs ni
    fixParam d = d

    -- | Heuristic: a function-pointer parameter "looks like" it carries a
    --   pointer value if the parameter name contains ptr/handle/addr
    --   substrings (case-insensitive).  Without full call-site analysis this
    --   is the best we can do for function-pointer declarators.
    paramLooksLikePtr :: (Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo)) -> Bool
    paramLooksLikePtr (Just (CDeclr (Just (Ident n _ _)) _ _ _ _), _, _) =
        let lower = map toLower n
        in any (`isInfixOf` lower) ["ptr", "pointer", "handle", "addr", "hdl"]
    paramLooksLikePtr _ = False
