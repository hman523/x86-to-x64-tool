module Analysis.BitManipulationTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.BitManipulation
import Analysis.IssueTypes

bitManipulationSpec :: Spec
bitManipulationSpec = describe "BitManipulation Analysis" $ do

    describe "checkPackingPtrsWithFlagsInInt" $ do
        shouldFlagError
            "flags cast to int of pointer OR'd with flags"
            "void foo() { int *p; int flags; int packed = (int)(p | flags); }"
            checkPackingPtrsWithFlagsInInt

        shouldFlagError
            "flags cast to unsigned int of pointer OR'd with flags"
            "void foo() { int *p; unsigned int flags; unsigned int packed = (unsigned int)(p | flags); }"
            checkPackingPtrsWithFlagsInInt

        shouldNotFlagError
            "does not flag int | int cast to int"
            "void foo() { int a; int b; int c = (int)(a | b); }"
            checkPackingPtrsWithFlagsInInt

        shouldFlagError
            "chained cast: (int)(long)(ptr | flags) still detected"
            "void foo() { int *p; int flags; int packed = (int)(long)(p | flags); }"
            checkPackingPtrsWithFlagsInInt

    describe "checkBitShiftsOnPtr" $ do
        shouldFlagError
            "flags left-shift on pointer-typed variable"
            "void foo() { int *p; int n; long x = p << n; }"
            checkBitShiftsOnPtr

        shouldFlagError
            "flags right-shift on pointer-typed variable"
            "void foo() { int *p; int n; long x = p >> n; }"
            checkBitShiftsOnPtr

        shouldNotFlagError
            "does not flag shift on int variable"
            "void foo() { int x = 1; int shift = x << 4; }"
            checkBitShiftsOnPtr

    describe "checkExtractingPtrBitsIn32BitVar" $ do
        shouldFlagError
            "flags cast to int of right-shifted pointer"
            "void foo() { int *p; int bits = (int)(p >> 32); }"
            checkExtractingPtrBitsIn32BitVar

        shouldFlagError
            "flags cast to unsigned int of right-shifted pointer"
            "void foo() { int *p; unsigned int bits = (unsigned int)(p >> 32); }"
            checkExtractingPtrBitsIn32BitVar

        shouldNotFlagError
            "does not flag cast to int of right-shifted int"
            "void foo() { long x; int bits = (int)(x >> 16); }"
            checkExtractingPtrBitsIn32BitVar

        shouldFlagError
            "chained cast: (int)(long)(ptr >> n) still detected"
            "void foo() { int *p; int bits = (int)(long)(p >> 32); }"
            checkExtractingPtrBitsIn32BitVar

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all three bit-manipulation checks fire in one function"
            "void foo() { int *p; int flags; int packed = (int)(p | flags); int n; long shifted = p << n; int bits = (int)(p >> 32); }"
            analyzeBitManipulationIssues
            [PackingPtrsWithFlagsInInt, BitShiftsOnPtr, ExtractingPtrBitsIn32BitVar]

        shouldFlagNIssues
            "two pointer shifts (left and right) produce exactly two BitShiftsOnPtr issues"
            "void foo() { int *p; int *q; int n; long x = p << n; long y = q >> 2; }"
            checkBitShiftsOnPtr
            2

        shouldFlagNIssues
            "two pointer-packing casts produce exactly two PackingPtrsWithFlagsInInt issues"
            "void foo() { int *p; int *q; int flags; int a = (int)(p | flags); int b = (int)(q | flags); }"
            checkPackingPtrsWithFlagsInInt
            2

    describe "edge cases" $ do

        -- XOR is semantically equivalent to OR for pointer-packing: upper bits are still lost
        shouldFlagError
            "flags (int)(ptr ^ flags): XOR of pointer and flags cast to int"
            "void foo() { int *p; int flags; int packed = (int)(p ^ flags); }"
            checkPackingPtrsWithFlagsInInt

        shouldNotFlagError
            "does not flag (int)(a ^ b) where both operands are int"
            "void foo() { int a; int b; int c = (int)(a ^ b); }"
            checkPackingPtrsWithFlagsInInt

        -- Shift by a compile-time constant (not a variable) on a pointer
        shouldFlagError
            "flags pointer left-shift by a literal constant"
            "void foo() { int *p; long x = p << 3; }"
            checkBitShiftsOnPtr

        shouldFlagError
            "flags pointer right-shift by a literal constant"
            "void foo() { int *p; long x = p >> 8; }"
            checkBitShiftsOnPtr

        -- Casting the result to long rather than int should NOT flag extracting-bits check
        shouldNotFlagError
            "does not flag (long)(ptr >> n): cast to long does not truncate pointer bits"
            "void foo() { int *p; int n; long x = (long)(p >> n); }"
            checkExtractingPtrBitsIn32BitVar

        -- AND on a pointer is a separate check (checkBitMaskingAssuming32bitPts), not here
        shouldNotFlagError
            "does not flag (int)(ptr & ~flags): AND is not shift/pack, handled elsewhere"
            "void foo() { int *p; int flags; int x = (int)(p & ~flags); }"
            checkBitShiftsOnPtr

        -- Extracting bits from the high end of a 64-bit pointer
        shouldFlagError
            "flags extracting high 32 bits of pointer: (unsigned int)(ptr >> 32)"
            "void foo() { int *p; unsigned int hi = (unsigned int)(p >> 32); }"
            checkExtractingPtrBitsIn32BitVar

        shouldFlagError
            "flags extracting bits via shift amount 48"
            "void foo() { int *p; int bits = (int)(p >> 48); }"
            checkExtractingPtrBitsIn32BitVar
