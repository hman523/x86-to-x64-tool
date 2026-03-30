module Transformation.MemoryAllocation where

import Language.C.Syntax.AST
import Analysis.UtilTypes

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
transformAllocationSizeCalcsMayOverflow _ issue = undefined

transformMallocWithoutOverflowChecking :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformMallocWithoutOverflowChecking _ issue = undefined

transformUsingIntToStoreAllocationSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntToStoreAllocationSizes _ issue = undefined
