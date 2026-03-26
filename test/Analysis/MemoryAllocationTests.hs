module Analysis.MemoryAllocationTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.MemoryAllocation
import Analysis.UtilTypes

memoryAllocationSpec :: Spec
memoryAllocationSpec = describe "MemoryAllocation Analysis" $ do

    describe "checkAllocationSizeCalculationsMayOverflow" $ do
        shouldFlagError
            "flags malloc with int*int size calculation"
            "void foo() { int n; int m; char *p = malloc(n * m); }"
            checkAllocationSizeCalculationsMayOverflow

        shouldNotFlagError
            "does not flag malloc with int*long size calculation"
            "void foo() { int n; long m; char *p = malloc(n * m); }"
            checkAllocationSizeCalculationsMayOverflow

        shouldNotFlagError
            "does not flag simple malloc with sizeof"
            "void foo() { char *p = malloc(sizeof(int)); }"
            checkAllocationSizeCalculationsMayOverflow

    describe "checkMallocWithoutOverflowChecking" $ do
        shouldFlagError
            "flags malloc with int+int size addition"
            "void foo() { int a; int b; char *p = malloc(a + b); }"
            checkMallocWithoutOverflowChecking

        shouldNotFlagError
            "does not flag malloc with int+long size addition"
            "void foo() { int a; long b; char *p = malloc(a + b); }"
            checkMallocWithoutOverflowChecking

    describe "checkUsingIntToStoreAllocationSizes" $ do
        shouldFlagError
            "flags sizeof result stored in int variable"
            "void foo() { int sz; int *p; sz = sizeof(*p); }"
            checkUsingIntToStoreAllocationSizes

        shouldNotFlagError
            "does not flag sizeof stored in long"
            "void foo() { long sz; int *p; sz = sizeof(*p); }"
            checkUsingIntToStoreAllocationSizes

        shouldNotFlagError
            "does not flag plain int assignment without sizeof"
            "void foo() { int x; int y = 5; x = y; }"
            checkUsingIntToStoreAllocationSizes
