module Transformation.Helpers where

import Data.Generics (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Position (posOf)

-- | Build a single typedef-name type specifier, e.g. @typedefSpec "intptr_t"@.
typedefSpec :: String -> CDeclarationSpecifier NodeInfo
typedefSpec name =
    CTypeSpec (CTypeDef (Ident name 0 undefNode) undefNode)

-- | Walk the AST and, at any CCast node whose NodeInfo matches the target
--   position, replace all type specifiers in the cast declaration with the
--   single supplied specifier (stripping derived declarators so the result
--   is a plain named type).
replaceCastType :: NodeInfo
                -> CDeclarationSpecifier NodeInfo
                -> CTranslUnit
                -> CTranslUnit
replaceCastType targetInfo newSpec = everywhere (mkT fixCast)
  where
    fixCast :: CExpression NodeInfo -> CExpression NodeInfo
    fixCast (CCast (CDecl _ declrs ni) inner info)
        | posOf info == posOf targetInfo
        = CCast (CDecl [newSpec] declrs ni) inner info
    fixCast e = e

-- | Walk the AST and, at any CDecl containing a declarator whose NodeInfo
--   matches the target position, replace all its type specifiers with the
--   single supplied specifier.
--
--   NOTE: @lookupDeclPos@ stores the @CDeclr@ NodeInfo (the inner
--   declarator), not the outer @CDecl@ NodeInfo, so we match against the
--   inner declarator positions.
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

    isTypeSpec (CTypeSpec _) = True
    isTypeSpec _             = False

    hasDeclrAt (Just (CDeclr _ _ _ _ dNi), _, _) = posOf dNi == posOf targetInfo
    hasDeclrAt _                                  = False

-- | Walk the AST and, at any @return expr@ statement whose NodeInfo matches
--   the target position, wrap @expr@ in a cast to @newSpec@.
--   e.g. @return ptr;@ becomes @return (intptr_t)ptr;@
wrapReturnExpr :: NodeInfo
               -> CDeclarationSpecifier NodeInfo
               -> CTranslUnit
               -> CTranslUnit
wrapReturnExpr targetInfo newSpec = everywhere (mkT fixReturn)
  where
    fixReturn :: CStatement NodeInfo -> CStatement NodeInfo
    fixReturn (CReturn (Just expr) info)
        | posOf info == posOf targetInfo
        = CReturn (Just (CCast (CDecl [newSpec] [] undefNode) expr undefNode)) info
    fixReturn s = s

-- | Walk the AST and, at any @va_arg(ap, T)@ expression whose NodeInfo
--   matches the target position, replace the type @T@ with @newSpec@.
replaceVaArgType :: NodeInfo
                 -> CDeclarationSpecifier NodeInfo
                 -> CTranslUnit
                 -> CTranslUnit
replaceVaArgType targetInfo newSpec = everywhere (mkT fixVaArg)
  where
    fixVaArg :: CExpression NodeInfo -> CExpression NodeInfo
    fixVaArg (CBuiltinExpr (CBuiltinVaArg ap (CDecl _ declrs ni) info))
        | posOf info == posOf targetInfo
        = CBuiltinExpr (CBuiltinVaArg ap (CDecl [newSpec] declrs ni) info)
    fixVaArg e = e
