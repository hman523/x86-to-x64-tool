module Linter.MemoryAllocationTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.MemoryAllocation
import Analysis.MemoryAllocation (analyzeMemoryAllocationIssues)
import Analysis.IssueTypes

memoryAllocationLintSpec :: Spec
memoryAllocationLintSpec = describe "MemoryAllocation Linting" $ do
  testLintAllocationSizeCalcsMayOverflow
  testLintMallocWithoutOverflowChecking
  testLintUsingIntToStoreAllocationSizes

testLintAllocationSizeCalcsMayOverflow :: Spec
testLintAllocationSizeCalcsMayOverflow =
  describe "lintAllocationSizeCalcsMayOverflow" $ do
    shouldLeaveUnresolved "leaves AllocationSizeCalcsMayOverflow unresolved"
      "void f() { int n = 4; int m = 8; void *p = malloc(n * m); }"
      analyzeMemoryAllocationIssues
      lintMemoryAllocationIssues
      [AllocationSizeCalcsMayOverflow]

testLintMallocWithoutOverflowChecking :: Spec
testLintMallocWithoutOverflowChecking =
  describe "lintMallocWithoutOverflowChecking" $ do
    shouldLeaveUnresolved "leaves MallocWithoutOverflowChecking unresolved"
      "void f() { int n = 4; int m = 8; void *p = malloc(n + m); }"
      analyzeMemoryAllocationIssues
      lintMemoryAllocationIssues
      [MallocWithoutOverflowChecking]

testLintUsingIntToStoreAllocationSizes :: Spec
testLintUsingIntToStoreAllocationSizes =
  describe "lintUsingIntToStoreAllocationSizes" $ do
    shouldLintTo "rewrites int variable storing sizeof result to size_t"
      "void f() { int n; n = sizeof(int); }"
      analyzeMemoryAllocationIssues
      lintMemoryAllocationIssues
      "size_t"
    shouldLintTo "rewrites unsigned int variable storing sizeof result to size_t"
      "void f() { unsigned int n; n = sizeof(int); }"
      analyzeMemoryAllocationIssues
      lintMemoryAllocationIssues
      "size_t"
