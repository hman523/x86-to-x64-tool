module Transformation.BitManipulation where

import Language.C.Syntax.AST
import Analysis.IssueTypes

transformBitManipulationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformBitManipulationIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      PackingPtrsWithFlagsInInt   -> transformPackingPtrsWithFlagsInInt   a issue
      BitShiftsOnPtr              -> transformBitShiftsOnPtr              a issue
      ExtractingPtrBitsIn32BitVar -> transformExtractingPtrBitsIn32BitVar a issue
      _                           -> (a, Just issue)

transformPackingPtrsWithFlagsInInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPackingPtrsWithFlagsInInt _ issue = undefined

transformBitShiftsOnPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformBitShiftsOnPtr _ issue = undefined

transformExtractingPtrBitsIn32BitVar :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformExtractingPtrBitsIn32BitVar _ issue = undefined
