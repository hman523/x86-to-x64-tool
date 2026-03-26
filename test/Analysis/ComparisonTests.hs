module Analysis.ComparisonTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Comparison
import Analysis.UtilTypes

comparisonSpec :: Spec
comparisonSpec = describe "Comparison Analysis" $ do

    describe "checkLoopCounterAsIntWhenIteratingOverPtrArrays" $ do
        shouldFlagError
            "flags for-loop with int counter bounded by pointer subtraction"
            "void foo() { int *start; int *end; for (int i = 0; i < (end - start); i++) { } }"
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

        shouldNotFlagError
            "does not flag fseek with long offset"
            "void foo() { long offset; fseek(0, offset, 0); }"
            checkUsingIntForFileOffsets

        shouldNotFlagError
            "does not flag non-seek function calls"
            "void foo() { int x; printf(\"%d\", x); }"
            checkUsingIntForFileOffsets
