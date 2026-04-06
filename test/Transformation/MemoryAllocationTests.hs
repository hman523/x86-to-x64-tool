module Transformation.MemoryAllocationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.MemoryAllocation
import Analysis.MemoryAllocation (analyzeMemoryAllocationIssues)
import Analysis.IssueTypes

memoryAllocationTransformSpec :: Spec
memoryAllocationTransformSpec = describe "MemoryAllocation Transformations" $ do
  testTransformAllocationSizeCalcsMayOverflow
  testTransformMallocWithoutOverflowChecking
  testTransformUsingIntToStoreAllocationSizes

testTransformAllocationSizeCalcsMayOverflow :: Spec
testTransformAllocationSizeCalcsMayOverflow =
  describe "transformAllocationSizeCalcsMayOverflow" $ do
    shouldLeaveUnresolved "leaves AllocationSizeCalcsMayOverflow unresolved"
      "void f() { int n = 4; int m = 8; void *p = malloc(n * m); }"
      analyzeMemoryAllocationIssues
      transformMemoryAllocationIssues
      [AllocationSizeCalcsMayOverflow]

testTransformMallocWithoutOverflowChecking :: Spec
testTransformMallocWithoutOverflowChecking =
  describe "transformMallocWithoutOverflowChecking" $ do
    shouldLeaveUnresolved "leaves MallocWithoutOverflowChecking unresolved"
      "void f() { int n = 4; int m = 8; void *p = malloc(n + m); }"
      analyzeMemoryAllocationIssues
      transformMemoryAllocationIssues
      [MallocWithoutOverflowChecking]

testTransformUsingIntToStoreAllocationSizes :: Spec
testTransformUsingIntToStoreAllocationSizes =
  describe "transformUsingIntToStoreAllocationSizes" $ do
    shouldTransformTo "rewrites int variable storing sizeof result to size_t"
      "void f() { int n; n = sizeof(int); }"
      analyzeMemoryAllocationIssues
      transformMemoryAllocationIssues
      "size_t"
