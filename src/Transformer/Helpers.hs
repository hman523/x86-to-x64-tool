module Transformer.Helpers
    ( typedefSpec
    , retypeDecl
    , retypeFunReturnType
    , hasExactlyOneLong
    , hasUnsignedSpec
    , isTypeSpec
    ) where

import Data.Generics          (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node   (NodeInfo, undefNode)
import Language.C.Data.Ident  (Ident(..))
import Language.C.Data.Position (posOf)

-- | Build a typedef-name type specifier, e.g. @typedefSpec "int32_t"@.
typedefSpec :: String -> CDeclarationSpecifier NodeInfo
typedefSpec name = CTypeSpec (CTypeDef (Ident name 0 undefNode) undefNode)

-- | Walk the AST and, at any CDecl containing a declarator whose NodeInfo
--   matches the target position, replace all its type specifiers with the
--   single supplied specifier.
--
--   Matches by comparing the declarator's source position, so this works for
--   local variables, function parameters, and global declarations alike.
retypeDecl :: NodeInfo
           -> CDeclarationSpecifier NodeInfo
           -> CTranslUnit
           -> CTranslUnit
retypeDecl targetInfo newSpec = everywhere (mkT fixDecl)
  where
    fixDecl :: CDeclaration NodeInfo -> CDeclaration NodeInfo
    fixDecl (CDecl specs declrs ni)
        | any hasDeclrAt declrs
        = CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni
    fixDecl d = d

    hasDeclrAt (Just (CDeclr _ _ _ _ dNi), _, _) = posOf dNi == posOf targetInfo
    hasDeclrAt _                                   = False

-- | Walk the AST and, at any 'CFunctionDef' whose own declarator NodeInfo
--   matches the target position, replace its return-type specifiers with the
--   single supplied specifier.
--
--   The return type of a C function is encoded in the @specs@ field of
--   'CFunDef', not in a 'CDeclaration', so it cannot be rewritten by
--   'retypeDecl'.  This helper handles that case.
retypeFunReturnType :: NodeInfo
                    -> CDeclarationSpecifier NodeInfo
                    -> CTranslUnit
                    -> CTranslUnit
retypeFunReturnType targetInfo newSpec = everywhere (mkT fixFunDef)
  where
    fixFunDef :: CFunctionDef NodeInfo -> CFunctionDef NodeInfo
    fixFunDef (CFunDef specs declr@(CDeclr _ _ _ _ dNi) params body fni)
        | posOf dNi == posOf targetInfo
        = CFunDef (newSpec : filter (not . isTypeSpec) specs) declr params body fni
    fixFunDef f = f

-- ---------------------------------------------------------------------------
-- Shared spec predicates
-- ---------------------------------------------------------------------------

-- | True when the specifier list denotes exactly @long@ or @unsigned long@
--   (one 'CLongType' present), but NOT @long long@ (two 'CLongType's) and
--   NOT @long double@ (a 'CDoubleType' or 'CFloatType' also present).
hasExactlyOneLong :: [CDeclarationSpecifier a] -> Bool
hasExactlyOneLong specs =
    length [() | CTypeSpec (CLongType _) <- specs] == 1
    && not (any isFloatingSpec specs)
  where
    isFloatingSpec (CTypeSpec (CDoubleType _)) = True
    isFloatingSpec (CTypeSpec (CFloatType  _)) = True
    isFloatingSpec _                           = False

-- | True when the specifier list contains an @unsigned@ qualifier.
hasUnsignedSpec :: [CDeclarationSpecifier a] -> Bool
hasUnsignedSpec = any isU
  where
    isU (CTypeSpec (CUnsigType _)) = True
    isU _                          = False

-- | True when the specifier is a type specifier (as opposed to storage class
--   or type qualifier).
isTypeSpec :: CDeclarationSpecifier a -> Bool
isTypeSpec (CTypeSpec _) = True
isTypeSpec _             = False
