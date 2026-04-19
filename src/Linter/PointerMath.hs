{-# LANGUAGE LambdaCase #-}
module Linter.PointerMath
  ( lintPointerMathIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintPointerMathIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintPointerMathIssues = dispatchLinter $ \case
    PtrDiffStoredAs32bit                    -> Just lintPtrDiffStoredAs32bit
    PointerAddOverflow                      -> Just lintPointerAddOverflow
    PtrSubUnderflow                         -> Just lintPtrSubUnderflow
    ArrayIndexingIntInArrayOver2tothe31size -> Just lintArrayIndexingIntInArrayOver2tothe31size
    _                                       -> Nothing

lintPtrDiffStoredAs32bit :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPtrDiffStoredAs32bit ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)

-- Cannot be done automatically: ptr + int_offset where the offset is int.
-- The offset variable may be used elsewhere with its current type, may be
-- negative (making size_t wrong), or may come from a complex expression with
-- no single declaration to retype. Choosing to cast at the call site, retype
-- the variable, or restructure the surrounding logic depends on context.
lintPointerAddOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPointerAddOverflow = unlintable

-- Cannot be done automatically: ptr - unsigned_val where the unsigned value
-- can exceed the pointer's valid range, causing underflow. The fix could be a
-- signed cast, a type change, or a bounds check; choosing incorrectly could
-- silently change behavior for large values, and the right choice depends on
-- whether negative offsets are valid in the caller's usage.
lintPtrSubUnderflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPtrSubUnderflow = unlintable

lintArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintArrayIndexingIntInArrayOver2tothe31size ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)
