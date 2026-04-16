-- | Rewrites @typedef long T@ and @typedef unsigned long T@ declarations to
--   semantically equivalent fixed-width typedef names.
--
--   On LP64 the size of @long@ changes from 4 to 8 bytes.  Any code that
--   defines a typedef alias over @long@ will silently inherit the wider type.
--   Rewriting the typedef base type to @int32_t@ / @uint32_t@ preserves the
--   original 32-bit size for all downstream uses of the alias without
--   requiring every usage site to be touched.
--
--   Only @typedef long T@ and @typedef unsigned long T@ are affected
--   (not @typedef long long T@ — which is already 64-bit on both models,
--   nor typedefs whose base is a pointer to @long@, which are covered by
--   the variable-retype pass when the pointer target type matters).
module Transformer.TypedefReplacement
    ( transformTypedefs
    ) where

import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo)
import Transformer.Helpers    (typedefSpec, retypeDecl, hasExactlyOneLong,
                               hasUnsignedSpec)

-- | Rewrite every @typedef long T@ / @typedef unsigned long T@ declaration
--   in the translation unit.
transformTypedefs :: CTranslUnit -> CTranslUnit
transformTypedefs ast@(CTranslUnit decls _) = foldl applyToDecl ast decls

applyToDecl :: CTranslUnit -> CExternalDeclaration NodeInfo -> CTranslUnit
applyToDecl ast (CDeclExt decl) = applyToTypedefDecl decl ast
applyToDecl ast _               = ast

-- | If the declaration is a @typedef long ...@, retype each declared name.
applyToTypedefDecl :: CDeclaration NodeInfo -> CTranslUnit -> CTranslUnit
applyToTypedefDecl (CDecl specs declrs _) ast
    | isTypedefDecl specs && hasExactlyOneLong specs =
        let newSpec = typedefSpec (if hasUnsignedSpec specs then "uint32_t" else "int32_t")
        in foldl (\a ni -> retypeDecl ni newSpec a) ast (collectDeclrNis declrs)
    | otherwise = ast
applyToTypedefDecl _ ast = ast

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isTypedefDecl :: [CDeclarationSpecifier a] -> Bool
isTypedefDecl = any isTypedefSpec
  where
    isTypedefSpec (CStorageSpec (CTypedef _)) = True
    isTypedefSpec _                           = False

collectDeclrNis :: [(Maybe (CDeclarator NodeInfo), b, c)] -> [NodeInfo]
collectDeclrNis declrs =
    [ ni | (Just (CDeclr _ _ _ _ ni), _, _) <- declrs ]
