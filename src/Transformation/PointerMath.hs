module Transformation.PointerMath where

import Language.C.Syntax.AST
import Analysis.UtilTypes

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
transformPtrDiffStoredAs32bit _ issue = undefined

transformPointerAddOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPointerAddOverflow _ issue = undefined

transformPtrSubUnderflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrSubUnderflow _ issue = undefined

transformArrayIndexingIntInArrayOver2tothe31size :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformArrayIndexingIntInArrayOver2tothe31size _ issue = undefined
