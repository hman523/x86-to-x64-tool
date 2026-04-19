{-# LANGUAGE LambdaCase #-}
module Linter.TypeSize
  ( lintTypeSizeIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintTypeSizeIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintTypeSizeIssues = dispatchLinter $ \case
    CastPointerToInt   -> Just lintCastPointerToInt
    CastPointerToUInt  -> Just lintCastPointerToUInt
    CastIntToPointer   -> Just lintCastIntToPointer
    CastLongToPointer  -> Just lintCastLongToPointer
    SizeOfIntIsVoid    -> Just lintSizeOfIntIsVoid
    SizeOfLongIsVoid   -> Just lintSizeOfLongIsVoid
    UsingIntAsSizet    -> Just lintUsingIntAsSizet
    UsingIntAsPtrdifft -> Just lintUsingIntAsPtrdifft
    UsingUIntAsMemSize -> Just lintUsingUIntAsMemSize
    _                  -> Nothing

-- ---------------------------------------------------------------------------
-- Cast rewrites
-- ---------------------------------------------------------------------------

-- | (int)ptr  ->  (intptr_t)ptr
--   Collapses any intermediate cast in a chain like @(int)(long)ptr@
--   so the result is @(intptr_t)ptr@, not @(intptr_t)(long)ptr@.
lintCastPointerToInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastPointerToInt ast issue =
    (replaceCastTypeCollapsing (issuePos issue) (typedefSpec "intptr_t") ast, Nothing)

-- | (unsigned int)ptr  ->  (uintptr_t)ptr
--   Collapses any intermediate cast in a chain like @(unsigned int)(long)ptr@.
lintCastPointerToUInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastPointerToUInt ast issue =
    (replaceCastTypeCollapsing (issuePos issue) (typedefSpec "uintptr_t") ast, Nothing)

-- Cannot be done automatically: (int*)x casts an integer to a pointer type.
-- Replacing the cast with (intptr_t)x would change the result from a pointer
-- to an integer, breaking any subsequent dereference.  The correct fix depends
-- on whether the code intends to form a valid pointer or just store a value.
lintCastIntToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastIntToPointer = unlintable

-- Cannot be done automatically: (T*)long casts a long to a pointer type.
-- Replacing with (intptr_t)long changes the result from pointer to integer,
-- breaking subsequent dereferences.  Requires understanding the code's intent.
lintCastLongToPointer :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintCastLongToPointer = unlintable

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
