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

        shouldFlagError
            "chained cast: (int*)(long)0xDEAD still detected"
            "void foo() { int *p = (int*)(long)0xDEAD; }"
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

    describe "edge cases" $ do

        -- checkMagicValuesUsed only flags literal 4 and 8 (pointer word width on 32-/64-bit);
        -- larger multiples are NOT flagged by this check.
        shouldFlagError
            "flags malloc(4) as a magic size (32-bit pointer width)"
            "void foo() { char *p = malloc(4); }"
            checkMagicValuesUsed

        shouldFlagError
            "flags malloc(8) as a magic size (64-bit pointer width)"
            "void foo() { char *p = malloc(8); }"
            checkMagicValuesUsed

        shouldNotFlagError
            "does not flag malloc(16) (not a recognised pointer-width magic number)"
            "void foo() { char *p = malloc(16); }"
            checkMagicValuesUsed

        -- Very small constant: malloc(1) is an odd but not an architecture-width assumption
        shouldNotFlagError
            "does not flag malloc(1) as a magic size"
            "void foo() { char *p = malloc(1); }"
            checkMagicValuesUsed

        -- checkBitMaskingAssuming32bitPts only flags the exact 32-bit all-ones mask 0xFFFFFFFF
        -- applied directly to a pointer (mirrors the passing test above)
        shouldFlagError
            "flags pointer AND 0xFFFFFFFF (32-bit all-ones truncating mask)"
            "void foo() { int *p; long x = (long)(p & 0xFFFFFFFF); }"
            checkBitMaskingAssuming32bitPts

        shouldNotFlagError
            "does not flag non-pointer AND 0xFFFFFFFF"
            "void foo() { long n = 0; long x = n & 0xFFFFFFFF; }"
            checkBitMaskingAssuming32bitPts

        -- (void*)-1 is a unary negation expression, not a CIntConst literal;
        -- checkHardCodedAddressValues only matches CConst integer literals.
        shouldNotFlagError
            "does not flag (void*)-1 (unary negation, not a literal integer cast)"
            "void foo() { void *p = (void*)-1; }"
            checkHardCodedAddressValues

        -- Casting through char*: still a hardcoded address
        shouldFlagError
            "flags cast of non-zero literal to char pointer"
            "void foo() { char *p = (char*)0x8000; }"
            checkHardCodedAddressValues

        -- NULL (zero) cast to pointer is the only valid literal pointer: must NOT flag
        shouldNotFlagError
            "does not flag (int*)0 (explicit null pointer constant)"
            "void foo() { int *p = (int*)0; }"
            checkHardCodedAddressValues
