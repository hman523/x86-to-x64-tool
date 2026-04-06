module Transformation.MemoryAllocation where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

transformMemoryAllocationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformMemoryAllocationIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      AllocationSizeCalcsMayOverflow -> transformAllocationSizeCalcsMayOverflow a issue
      MallocWithoutOverflowChecking  -> transformMallocWithoutOverflowChecking  a issue
      UsingIntToStoreAllocationSizes -> transformUsingIntToStoreAllocationSizes a issue
      _                              -> (a, Just issue)

transformAllocationSizeCalcsMayOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAllocationSizeCalcsMayOverflow ast issue = (ast, Just issue)

transformMallocWithoutOverflowChecking :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformMallocWithoutOverflowChecking ast issue = (ast, Just issue)

transformUsingIntToStoreAllocationSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntToStoreAllocationSizes ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)
