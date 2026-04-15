module Linter.TypeSize where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintTypeSizeIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintTypeSizeIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      CastPointerToInt   -> lintCastPointerToInt   a issue
      CastPointerToUInt  -> lintCastPointerToUInt  a issue
      CastIntToPointer   -> lintCastIntToPointer   a issue
      CastLongToPointer  -> lintCastLongToPointer  a issue
      SizeOfIntIsVoid    -> lintSizeOfIntIsVoid    a issue
      SizeOfLongIsVoid   -> lintSizeOfLongIsVoid   a issue
      UsingIntAsSizet    -> lintUsingIntAsSizet    a issue
      UsingIntAsPtrdifft -> lintUsingIntAsPtrdifft a issue
      UsingUIntAsMemSize -> lintUsingUIntAsMemSize a issue
      _                  -> (a, Just issue)

-- ---------------------------------------------------------------------------
-- Cast rewrites
-- ---------------------------------------------------------------------------

-- | (int)ptr  ->  (intptr_t)ptr
lintCastPointerToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastPointerToInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | (unsigned int)ptr  ->  (uintptr_t)ptr
lintCastPointerToUInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastPointerToUInt ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "uintptr_t") ast, Nothing)

-- | (int*)x  ->  (intptr_t)x  [keeps the inner value; caller must cast to ptr]
lintCastIntToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastIntToPointer ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | (T*)long  ->  (intptr_t)long
lintCastLongToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastLongToPointer ast issue =
    (replaceCastType (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- Cannot be done automatically: sizeof(int) == sizeof(void*) was true on
-- 32-bit but is false on 64-bit. The condition guards code that assumes
-- pointer-sized ints; the branch taken on 32-bit may now need to become the
-- unconditional path, or the entire guarded block may need redesigning. The
-- tool cannot know which branch reflects the intended 64-bit behavior.
lintSizeOfIntIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintSizeOfIntIsVoid = unlintable

-- Cannot be done automatically: sizeof(long) == sizeof(void*) is true on
-- 64-bit Linux (LP64) but false on 64-bit Windows (LLP64). The correct fix
-- is to use intptr_t on both platforms, but only after understanding whether
-- the code's correctness assumption targets Linux or Windows specifically.
lintSizeOfLongIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintSizeOfLongIsVoid = unlintable

-- ---------------------------------------------------------------------------
-- Declaration retyping
-- ---------------------------------------------------------------------------

-- | int x = sizeof(...)  ->  size_t x = sizeof(...)
--   Only rewrites the declaration; triggered by issueDeclPos.
lintUsingIntAsSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUsingIntAsSizet ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)

-- | int x = p1 - p2  ->  ptrdiff_t x = p1 - p2
lintUsingIntAsPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUsingIntAsPtrdifft ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)

-- | unsigned int passed to malloc: retype the variable's declaration to
--   size_t when the argument is a plain variable (issueDeclPos is set).
--   Leaves unresolved when the argument is a complex expression.
lintUsingUIntAsMemSize :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUsingUIntAsMemSize ast issue =
    case issueDeclPos issue of
        Nothing -> (ast, Just issue)
        Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)

-- Helpers (typedefSpec, replaceCastType, retypeDecl) are re-exported
-- from Linter.Helpers via the import above.
