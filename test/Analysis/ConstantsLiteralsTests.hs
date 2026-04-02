module Analysis.ConstantsLiteralsTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.ConstantsLiterals
import Analysis.IssueTypes

constantsLiteralsSpec :: Spec
constantsLiteralsSpec = describe "ConstantsLiterals Analysis" $ do

    describe "checkMagicValuesUsed" $ do
        shouldFlagError
            "flags malloc(4) as magic pointer-size assumption"
            "void foo() { char *p = malloc(4); }"
            checkMagicValuesUsed

        shouldFlagError
            "flags malloc(8) as magic pointer-size assumption"
            "void foo() { char *p = malloc(8); }"
            checkMagicValuesUsed

        shouldNotFlagError
            "does not flag malloc with sizeof"
            "void foo() { char *p = malloc(sizeof(int)); }"
            checkMagicValuesUsed

        shouldNotFlagError
            "does not flag malloc with other literal sizes"
            "void foo() { char *p = malloc(100); }"
            checkMagicValuesUsed

    describe "checkBitMaskingAssuming32bitPts" $ do
        shouldFlagError
            "flags pointer AND 0xFFFFFFFF"
            "void foo() { int *p; long x = (long)(p & 0xFFFFFFFF); }"
            checkBitMaskingAssuming32bitPts

        shouldNotFlagError
            "does not flag int AND 0xFFFFFFFF"
            "void foo() { long n; long x = n & 0xFFFFFFFF; }"
            checkBitMaskingAssuming32bitPts

    describe "checkHardCodedAddressValues" $ do
        shouldFlagError
            "flags cast of non-zero integer literal to pointer"
            "void foo() { int *p = (int*)0xDEADBEEF; }"
            checkHardCodedAddressValues

        shouldNotFlagError
            "does not flag cast of 0 (NULL) to pointer"
            "void foo() { int *p = (int*)0; }"
            checkHardCodedAddressValues

    describe "checkConstantsUsedForSizeCalcs" $ do
        shouldFlagError
            "flags literal assigned to unsigned long (size_t) variable"
            "void foo() { unsigned long sz; sz = 64; }"
            checkConstantsUsedForSizeCalcs

        shouldNotFlagError
            "does not flag literal assigned to signed int"
            "void foo() { int x; x = 64; }"
            checkConstantsUsedForSizeCalcs

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all four constants-and-literals checks fire in one function"
            "void foo() { int *p; char *q = malloc(4); int *r = (int*)0xDEAD; long x = (long)(p & 0xFFFFFFFF); unsigned long sz; sz = 128; }"
            analyzeConstantsLiteralsIssues
            [MagicValuesUsed, HardCodedAddressValues, BitMaskingAssuming32bitPts, ConstantsUsedForSizeCalcs]

        shouldFlagNIssues
            "malloc(4) and malloc(8) each produce one MagicValuesUsed issue"
            "void foo() { char *p = malloc(4); char *q = malloc(8); }"
            checkMagicValuesUsed
            2

        shouldFlagNIssues
            "two hardcoded address casts produce exactly two HardCodedAddressValues issues"
            "void foo() { int *p = (int*)0x1000; int *q = (int*)0xDEAD; }"
            checkHardCodedAddressValues
            2
