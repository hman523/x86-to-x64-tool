module Transformation.PointerMath where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

transformPointerMathIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformPointerMathIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      PtrDiffStoredAs32bit                    -> transformPtrDiffStoredAs32bit                    a issue
      PointerAddOverflow                      -> transformPointerAddOverflow                      a issue
      PtrSubUnderflow                         -> transformPtrSubUnderflow                         a issue
      ArrayIndexingIntInArrayOver2tothe31size -> transformArrayIndexingIntInArrayOver2tothe31size a issue
      _                                       -> (a, Just issue)

transformPtrDiffStoredAs32bit :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrDiffStoredAs32bit ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)

-- Cannot be done automatically: ptr + int_offset where the offset is int.
-- The offset variable may be used elsewhere with its current type, may be
-- negative (making size_t wrong), or may come from a complex expression with
-- no single declaration to retype. Choosing to cast at the call site, retype
-- the variable, or restructure the surrounding logic depends on context.
transformPointerAddOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPointerAddOverflow = untransformable

-- Cannot be done automatically: ptr - unsigned_val where the unsigned value
-- can exceed the pointer's valid range, causing underflow. The fix could be a
-- signed cast, a type change, or a bounds check; choosing incorrectly could
-- silently change behavior for large values, and the right choice depends on
-- whether negative offsets are valid in the caller's usage.
transformPtrSubUnderflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrSubUnderflow = untransformable

transformArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformArrayIndexingIntInArrayOver2tothe31size ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)
