module Analysis.PlatformSpecificsTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.PlatformSpecifics
import Analysis.IssueTypes

platformSpecificsSpec :: Spec
platformSpecificsSpec = describe "PlatformSpecifics Analysis" $ do

    describe "checkAsmBlocks" $ do
        shouldFlagError
            "flags inline assembly blocks"
            "void foo() { __asm__(\"nop\"); }"
            checkAsmBlocks

        shouldNotFlagError
            "does not flag normal C code"
            "void foo() { int x = 1 + 2; }"
            checkAsmBlocks

    describe "checkInlineAsmWithx86Instructions" $ do
        shouldFlagError
            "flags asm with x86 register names"
            "void foo() { __asm__(\"movl %eax, %ebx\"); }"
            checkInlineAsmWithx86Instructions

        shouldFlagError
            "flags asm with bare eax reference"
            "void foo() { __asm__(\"xor eax, eax\"); }"
            checkInlineAsmWithx86Instructions

        shouldNotFlagError
            "does not flag asm without x86 registers"
            "void foo() { __asm__(\"nop\"); }"
            checkInlineAsmWithx86Instructions

    describe "checkx86SpecificCompilerIntrinsics" $ do
        shouldFlagError
            "flags _mm_ SSE intrinsic usage"
            "void foo() { _mm_set1_ps(0.0f); }"
            checkx86SpecificCompilerIntrinsics

        shouldFlagError
            "flags _mm256_ AVX intrinsic usage"
            "void foo() { _mm256_setzero_ps(); }"
            checkx86SpecificCompilerIntrinsics

        shouldNotFlagError
            "does not flag regular function calls"
            "void foo() { printf(\"hello\"); }"
            checkx86SpecificCompilerIntrinsics

    describe "checkAssumptionsAboutRegSizes" $ do
        shouldFlagError
            "flags sizeof(int) == 4 comparison"
            "void foo() { if (sizeof(int) == 4) { } }"
            checkAssumptionsAboutRegSizes

        shouldNotFlagError
            "does not flag sizeof(int) == 8 comparison"
            "void foo() { if (sizeof(int) == 8) { } }"
            checkAssumptionsAboutRegSizes

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all four platform checks fire from one function"
            "void foo() { __asm__(\"movl %eax, %ebx\"); if (sizeof(int) == 4) { } _mm_set1_ps(0); }"
            analyzePlatformSpecificIssues
            [AsmBlocks, InlineAsmWithx86Instructions, AssumptionsAboutRegSizes, X86SpecificCompilerIntrinsics]

        shouldFlagNIssues
            "two distinct asm blocks produce exactly two AsmBlocks issues"
            "void foo() { __asm__(\"nop\"); __asm__(\"nop\"); }"
            checkAsmBlocks
            2

        shouldFlagNIssues
            "two x86 intrinsic calls produce exactly two X86SpecificCompilerIntrinsics issues"
            "void foo() { _mm_set1_ps(0); _mm256_setzero_ps(); }"
            checkx86SpecificCompilerIntrinsics
            2

    describe "checkHandleTypesCastToInt" $ do
        shouldFlagError
            "flags HANDLE cast to int"
            "typedef void* HANDLE; void foo() { HANDLE h; int x = (int)h; }"
            checkHandleTypesCastToInt

        shouldFlagError
            "flags HANDLE cast to unsigned int"
            "typedef void* HANDLE; void foo() { HANDLE h; unsigned int x = (unsigned int)h; }"
            checkHandleTypesCastToInt

        shouldNotFlagError
            "does not flag HANDLE cast to void* (correct fix direction)"
            "typedef void* HANDLE; void foo() { HANDLE h; void *x = (void*)h; }"
            checkHandleTypesCastToInt
