module Analysis.ComparisonTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Comparison
import Analysis.IssueTypes

comparisonSpec :: Spec
comparisonSpec = describe "Comparison Analysis" $ do

    describe "checkLoopCounterAsIntWhenIteratingOverPtrArrays" $ do
        shouldFlagError
            "flags for-loop with int counter bounded by pointer subtraction"
            "void foo() { int *start; int *end; for (int i = 0; i < (end - start); i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

        shouldFlagError
            "flags for-loop with unsigned int counter bounded by pointer subtraction"
            "void foo() { int *start; int *end; for (unsigned int i = 0; i < (end - start); i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

        shouldNotFlagError
            "does not flag for-loop with int counter bounded by simple int"
            "void foo() { int n; for (int i = 0; i < n; i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

    describe "checkPtrComparisonWithIntConsts" $ do
        shouldFlagError
            "flags pointer compared with non-zero integer literal using <"
            "void foo() { int *p; if (p < 4096) { } }"
            checkPtrComparisonWithIntConsts

        shouldFlagError
            "flags pointer compared with integer literal using >"
            "void foo() { int *p; if (p > 100) { } }"
            checkPtrComparisonWithIntConsts

        shouldNotFlagError
            "does not flag pointer equality comparison with NULL (zero)"
            "void foo() { int *p; if (p == 0) { } }"
            checkPtrComparisonWithIntConsts

        shouldNotFlagError
            "does not flag int compared with int constant"
            "void foo() { int x; if (x < 100) { } }"
            checkPtrComparisonWithIntConsts

    describe "checkUsingIntForFileOffsets" $ do
        shouldFlagError
            "flags fseek with int offset variable"
            "void foo() { int offset; fseek(0, offset, 0); }"
            checkUsingIntForFileOffsets

        shouldFlagError
            "flags fseek with unsigned int offset variable"
            "void foo() { unsigned int offset; fseek(0, offset, 0); }"
            checkUsingIntForFileOffsets

        shouldNotFlagError
            "does not flag fseek with long offset"
            "void foo() { long offset; fseek(0, offset, 0); }"
            checkUsingIntForFileOffsets

        shouldNotFlagError
            "does not flag non-seek function calls"
            "void foo() { int x; printf(\"%d\", x); }"
            checkUsingIntForFileOffsets

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all three comparison checks fire together"
            "void foo() { int *start; int *end; for (int i = 0; i < (end - start); i++) { } int *p; if (p < 4096) { } int offset; fseek(0, offset, 0); }"
            analyzeComparisonIssues
            [LoopCounterAsIntWhenIteratingOverPtrArrays, PtrComparisonWithIntConsts, UsingIntForFileOffsets]

        shouldFlagNIssues
            "two fseek calls with int offsets produce exactly two issues"
            "void foo() { int off1; int off2; fseek(0, off1, 0); fseek(0, off2, 0); }"
            checkUsingIntForFileOffsets
            2

        shouldFlagNIssues
            "two pointer-vs-integer-constant comparisons produce exactly two issues"
            "void foo() { int *p; int *q; if (p < 4096) { } if (q > 100) { } }"
            checkPtrComparisonWithIntConsts
            2

    describe "compound loop condition edge cases" $ do

        shouldFlagError
            "int counter in compound condition: i < (end - start) && i < 100"
            "void foo() { int *start; int *end; \
            \for (int i = 0; i < (end - start) && i < 100; i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

        shouldFlagError
            "int counter with pointer subtraction on right side of &&"
            "void foo() { int *start; int *end; \
            \for (int i = 0; i < 100 && i < (end - start); i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

    describe "comparison edge cases" $ do

        -- while loop: counter is declared outside, condition has pointer subtraction
        -- The checker only handles 'for' loops with a built-in declaration; this is a gap.
        shouldFlagError
            "flags int loop variable bounded by pointer subtraction in while condition"
            "void foo() { int *start; int *end; int i = 0; while (i < (end - start)) { i++; } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

        -- != with non-zero literal is an invalid pointer comparison too
        shouldFlagError
            "flags pointer != non-zero integer literal"
            "void foo() { int *p; if (p != 4096) { } }"
            checkPtrComparisonWithIntConsts

        -- <= with non-zero literal
        shouldFlagError
            "flags pointer <= non-zero integer literal"
            "void foo() { int *p; if (p <= 4096) { } }"
            checkPtrComparisonWithIntConsts

        -- == 0 is a null check: valid, must NOT be flagged
        shouldNotFlagError
            "does not flag pointer == 0 (null pointer check)"
            "void foo() { int *p; if (p == 0) { } }"
            checkPtrComparisonWithIntConsts

        -- != 0 is a non-null check: valid, must NOT be flagged
        shouldNotFlagError
            "does not flag pointer != 0 (non-null check)"
            "void foo() { int *p; if (p != 0) { } }"
            checkPtrComparisonWithIntConsts

        -- fseek with a cast int offset
        shouldFlagError
            "flags fseek where int variable is explicitly cast before passing"
            "void foo() { int offset; fseek(0, (int)offset, 0); }"
            checkUsingIntForFileOffsets

        -- lseek should also be caught
        shouldFlagError
            "flags lseek with int offset variable"
            "void foo() { int offset; lseek(0, offset, 0); }"
            checkUsingIntForFileOffsets

        shouldNotFlagError
            "long counter in compound condition is not flagged"
            "void foo() { int *start; int *end; \
            \for (long i = 0; i < (end - start) && i < 100; i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays

        shouldFlagError
            "int counter with pointer subtraction nested in || condition"
            "void foo() { int *start; int *end; int limit; \
            \for (int i = 0; i < (end - start) || i < limit; i++) { } }"
            checkLoopCounterAsIntWhenIteratingOverPtrArrays
