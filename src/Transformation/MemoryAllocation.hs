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

-- Cannot be done automatically: malloc(n * m) where both operands are int can
-- overflow before widening to size_t. The fix (using calloc(n, m), casting one
-- operand, or adding an overflow check) each changes the program's API usage
-- or control flow. Choosing the right approach requires understanding the
-- programmer's intent and how the failure case should be handled.
transformAllocationSizeCalcsMayOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformAllocationSizeCalcsMayOverflow = untransformable

-- Cannot be done automatically: malloc(a + b) where both operands are int can
-- wrap before reaching size_t width. The correct fix requires an explicit
-- overflow guard, but the programmer must decide what to do on overflow
-- (abort, return NULL, clamp, etc.), which is a policy decision the tool
-- cannot make.
transformMallocWithoutOverflowChecking :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformMallocWithoutOverflowChecking = untransformable

transformUsingIntToStoreAllocationSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUsingIntToStoreAllocationSizes ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)
