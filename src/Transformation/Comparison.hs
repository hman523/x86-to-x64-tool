module Transformation.Comparison where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

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
transformLoopCounterAsIntWhenIteratingOverPtrArrays ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "ptrdiff_t") ast, Nothing)
    Nothing -> (ast, Just issue)

transformPtrComparisonWithIntConsts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrComparisonWithIntConsts ast issue = (ast, Just issue)

transformUsingIntForFileOffsets :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntForFileOffsets ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "off_t") ast, Nothing)
    Nothing -> (ast, Just issue)
