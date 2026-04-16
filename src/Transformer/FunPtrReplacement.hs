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
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)

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
            let newSpec  = typedefSpec (if hasUnsignedSpec specs
                                        then "uint32_t"
                                        else "int32_t")
                newSpecs = newSpec : filter (not . isTypeSpec) specs
            in CDecl newSpecs declrs ni
    fixParam d = d
