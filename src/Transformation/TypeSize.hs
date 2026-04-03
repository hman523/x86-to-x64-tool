module Transformation.TypeSize where

import Data.Generics (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo, undefNode)
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Position (posOf)
import Analysis.IssueTypes

transformTypeSizeIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformTypeSizeIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      CastPointerToInt   -> transformCastPointerToInt   a issue
      CastPointerToUInt  -> transformCastPointerToUInt  a issue
      CastIntToPointer   -> transformCastIntToPointer   a issue
      CastLongToPointer  -> transformCastLongToPointer  a issue
      SizeOfIntIsVoid    -> transformSizeOfIntIsVoid    a issue
      SizeOfLongIsVoid   -> transformSizeOfLongIsVoid   a issue
      UsingIntAsSizet    -> transformUsingIntAsSizet    a issue
      UsingIntAsPtrdifft -> transformUsingIntAsPtrdifft a issue
      UsingUIntAsMemSize -> transformUsingUIntAsMemSize a issue
      _                  -> (a, Just issue)

-- ---------------------------------------------------------------------------
-- Cast rewrites
-- ---------------------------------------------------------------------------

-- | (int)ptr  ->  (intptr_t)ptr
transformCastPointerToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastPointerToInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | (unsigned int)ptr  ->  (uintptr_t)ptr
transformCastPointerToUInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastPointerToUInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "uintptr_t") ast, Nothing)

-- | (int*)x  ->  (intptr_t)x  [keeps the inner value; caller must cast to ptr]
transformCastIntToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastIntToPointer ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | (T*)long  ->  (intptr_t)long
transformCastLongToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformCastLongToPointer ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | sizeof comparisons can't be auto-fixed: leave unresolved.
transformSizeOfIntIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfIntIsVoid ast issue = (ast, Just issue)

transformSizeOfLongIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfLongIsVoid ast issue = (ast, Just issue)

-- ---------------------------------------------------------------------------
-- Declaration retyping
-- ---------------------------------------------------------------------------

-- | int x = sizeof(...)  ->  size_t x = sizeof(...)
--   Only rewrites the declaration; triggered by issueDeclPos.
transformUsingIntAsSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntAsSizet ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)

-- | int x = p1 - p2  ->  ptrdiff_t x = p1 - p2
transformUsingIntAsPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntAsPtrdifft ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)

-- | unsigned int passed to malloc: retype the variable's declaration to
--   size_t when the argument is a plain variable (issueDeclPos is set).
--   Leaves unresolved when the argument is a complex expression.
transformUsingUIntAsMemSize :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingUIntAsMemSize ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)

-- ---------------------------------------------------------------------------
-- AST rewriting helpers
-- ---------------------------------------------------------------------------

-- | Build a single typedef-name type specifier for the given name,
--   e.g. @typedefSpec "intptr_t"@ produces the spec for @intptr_t@.
typedefSpec :: String -> CDeclarationSpecifier NodeInfo
typedefSpec name =
    CTypeSpec (CTypeDef (Ident name 0 undefNode) undefNode)

-- | Walk the AST and, at any CCast node whose NodeInfo matches the target
--   position, replace all type specifiers in the cast declaration with the
--   single supplied specifier (stripping derived declarators so the result is
--   a plain named type).
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
--   NOTE: @lookupDeclPos@ stores the @CDeclr@ NodeInfo, not the @CDecl@
--   NodeInfo, so we match against the inner declarator positions.
retypeDecl :: NodeInfo
           -> CDeclarationSpecifier NodeInfo
           -> CTranslUnit
           -> CTranslUnit
retypeDecl targetInfo newSpec = everywhere (mkT fixDecl)
  where
    fixDecl :: CDeclaration NodeInfo -> CDeclaration NodeInfo
    fixDecl (CDecl specs declrs ni)
        | any hasDeclrAt declrs
        -- Keep only non-type specifiers (storage class, qualifiers, etc.)
        -- and prepend the new type spec.
        = CDecl (newSpec : filter (not . isTypeSpec) specs) declrs ni
    fixDecl d = d

    isTypeSpec (CTypeSpec _) = True
    isTypeSpec _             = False

    hasDeclrAt (Just (CDeclr _ _ _ _ dNi), _, _) = posOf dNi == posOf targetInfo
    hasDeclrAt _                                  = False
