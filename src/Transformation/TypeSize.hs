module Transformation.TypeSize where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

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

-- Cannot be done automatically: sizeof(int) == sizeof(void*) was true on
-- 32-bit but is false on 64-bit. The condition guards code that assumes
-- pointer-sized ints; the branch taken on 32-bit may now need to become the
-- unconditional path, or the entire guarded block may need redesigning. The
-- tool cannot know which branch reflects the intended 64-bit behavior.
transformSizeOfIntIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfIntIsVoid = untransformable

-- Cannot be done automatically: sizeof(long) == sizeof(void*) is true on
-- 64-bit Linux (LP64) but false on 64-bit Windows (LLP64). The correct fix
-- is to use intptr_t on both platforms, but only after understanding whether
-- the code's correctness assumption targets Linux or Windows specifically.
transformSizeOfLongIsVoid :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeOfLongIsVoid = untransformable

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

-- Helpers (typedefSpec, replaceCastType, retypeDecl) are re-exported
-- from Transformation.Helpers via the import above.
