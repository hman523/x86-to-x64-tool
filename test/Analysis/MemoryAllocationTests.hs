module Analysis.MemoryAllocationTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.MemoryAllocation
import Analysis.IssueTypes

memoryAllocationSpec :: Spec
memoryAllocationSpec = describe "MemoryAllocation Analysis" $ do

    describe "checkAllocationSizeCalcsMayOverflow" $ do
        shouldFlagError
            "flags malloc with int*int size calculation"
            "void foo() { int n; int m; char *p = malloc(n * m); }"
            checkAllocationSizeCalcsMayOverflow

        shouldFlagError
            "flags malloc with unsigned int*unsigned int size calculation"
            "void foo() { unsigned int n; unsigned int m; char *p = malloc(n * m); }"
            checkAllocationSizeCalcsMayOverflow

        shouldNotFlagError
            "does not flag malloc with int*long size calculation"
            "void foo() { int n; long m; char *p = malloc(n * m); }"
            checkAllocationSizeCalcsMayOverflow

        shouldNotFlagError
            "does not flag simple malloc with sizeof"
            "void foo() { char *p = malloc(sizeof(int)); }"
            checkAllocationSizeCalcsMayOverflow

    describe "checkMallocWithoutOverflowChecking" $ do
        shouldFlagError
            "flags malloc with int+int size addition"
            "void foo() { int a; int b; char *p = malloc(a + b); }"
            checkMallocWithoutOverflowChecking

        shouldFlagError
            "flags malloc with unsigned int+unsigned int size addition"
            "void foo() { unsigned int a; unsigned int b; char *p = malloc(a + b); }"
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

        shouldFlagError
            "flags sizeof result stored in unsigned int variable"
            "void foo() { unsigned int sz; int *p; sz = sizeof(*p); }"
            checkUsingIntToStoreAllocationSizes

        shouldNotFlagError
            "does not flag sizeof stored in long"
            "void foo() { long sz; int *p; sz = sizeof(*p); }"
            checkUsingIntToStoreAllocationSizes

        shouldNotFlagError
            "does not flag plain int assignment without sizeof"
            "void foo() { int x; int y = 5; x = y; }"
            checkUsingIntToStoreAllocationSizes

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all three allocation checks fire in one function"
            "void foo() { int n; int m; char *p = malloc(n * m); char *q = malloc(n + m); int sz; int *r; sz = sizeof(*r); }"
            analyzeMemoryAllocationIssues
            [AllocationSizeCalcsMayOverflow, MallocWithoutOverflowChecking, UsingIntToStoreAllocationSizes]

        shouldFlagNIssues
            "two multiply-overflow malloc calls produce exactly two issues"
            "void foo() { int n; int m; int p; int q; char *a = malloc(n * m); char *b = malloc(p * q); }"
            checkAllocationSizeCalcsMayOverflow
            2

        shouldFlagNIssues
            "two sizeof-to-int assignments produce exactly two issues"
            "void foo() { int sz1; int sz2; int *p; int *q; sz1 = sizeof(*p); sz2 = sizeof(*q); }"
            checkUsingIntToStoreAllocationSizes
            2

    describe "edge cases" $ do

        -- calloc(n, m) semantically computes n*m, but the checker sees two separate
        -- arguments, not a multiplication expression — this tests that coverage gap.
        shouldFlagError
            "flags calloc(n, m) where both args are int: implicit n*m overflow risk"
            "void foo() { int n; int m; char *p = calloc(n, m); }"
            checkAllocationSizeCalcsMayOverflow

        -- realloc with an int+int addition in the size argument
        shouldFlagError
            "flags realloc(ptr, a+b) where both addends are int"
            "void foo() { void *ptr; int a; int b; ptr = realloc(ptr, a + b); }"
            checkMallocWithoutOverflowChecking

        -- Triple multiply: malloc(a * b * c) — the inner CBinary is a*b, outer is (*c)
        shouldFlagError
            "flags malloc(a * b * c) with all int operands: nested overflow"
            "void foo() { int a; int b; int c; char *p = malloc(a * b * c); }"
            checkAllocationSizeCalcsMayOverflow

        -- Mixing size_t with int: size_t * int — the result is size_t so no overflow
        shouldNotFlagError
            "does not flag malloc(sz * sizeof(char)) where sz is unsigned long (size_t)"
            "void foo() { unsigned long sz; char *p = malloc(sz * sizeof(char)); }"
            checkAllocationSizeCalcsMayOverflow
