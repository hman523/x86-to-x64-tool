{-# LANGUAGE LambdaCase #-}
module Linter.MemoryAllocation
  ( lintMemoryAllocationIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintMemoryAllocationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintMemoryAllocationIssues = dispatchLinter $ \case
    AllocationSizeCalcsMayOverflow -> Just lintAllocationSizeCalcsMayOverflow
    MallocWithoutOverflowChecking  -> Just lintMallocWithoutOverflowChecking
    UsingIntToStoreAllocationSizes -> Just lintUsingIntToStoreAllocationSizes
    _                              -> Nothing

-- Cannot be done automatically: malloc(n * m) where both operands are int can
-- overflow before widening to size_t. The fix (using calloc(n, m), casting one
-- operand, or adding an overflow check) each changes the program's API usage
-- or control flow. Choosing the right approach requires understanding the
-- programmer's intent and how the failure case should be handled.
lintAllocationSizeCalcsMayOverflow :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintAllocationSizeCalcsMayOverflow = unlintable

-- Cannot be done automatically: malloc(a + b) where both operands are int can
-- wrap before reaching size_t width. The correct fix requires an explicit
-- overflow guard, but the programmer must decide what to do on overflow
-- (abort, return NULL, clamp, etc.), which is a policy decision the tool
-- cannot make.
lintMallocWithoutOverflowChecking :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintMallocWithoutOverflowChecking = unlintable

lintUsingIntToStoreAllocationSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUsingIntToStoreAllocationSizes ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)
