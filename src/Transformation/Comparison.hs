module Transformation.Comparison where

import Language.C.Syntax.AST
import Analysis.UtilTypes

transformComparisonIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformComparisonIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      LoopCounterAsIntWhenIteratingOverPtrArrays -> transformLoopCounterAsIntWhenIteratingOverPtrArrays a issue
      PtrComparisonWithIntConsts                 -> transformPtrComparisonWithIntConsts                 a issue
      UsingIntForFileOffsets                     -> transformUsingIntForFileOffsets                     a issue
      _                                          -> (a, Just issue)

transformLoopCounterAsIntWhenIteratingOverPtrArrays :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformLoopCounterAsIntWhenIteratingOverPtrArrays _ issue = undefined

transformPtrComparisonWithIntConsts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrComparisonWithIntConsts _ issue = undefined

transformUsingIntForFileOffsets :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntForFileOffsets _ issue = undefined
