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

transformPointerAddOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPointerAddOverflow ast issue = (ast, Just issue)

transformPtrSubUnderflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrSubUnderflow ast issue = (ast, Just issue)

transformArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformArrayIndexingIntInArrayOver2tothe31size ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)
