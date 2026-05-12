module Linter.PlatformSpecificsTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.PlatformSpecifics
import Analysis.PlatformSpecifics (analyzePlatformSpecificIssues)
import Analysis.IssueTypes

platformSpecificsLintSpec :: Spec
platformSpecificsLintSpec = describe "PlatformSpecifics Linting" $ do
  testLintInlineAsmWithx86Instructions
  testLintAsmBlocks
  testLintHandleTypesCastToInt
  testLintHandleTypesCastToUInt
  testLintX86SpecificCompilerIntrinsics
  testLintAssumptionsAboutRegSizes

testLintInlineAsmWithx86Instructions :: Spec
testLintInlineAsmWithx86Instructions =
  describe "lintInlineAsmWithx86Instructions" $ do
    shouldLeaveUnresolved "leaves InlineAsmWithx86Instructions unresolved"
      "void f() { __asm__(\"movl %eax, %ebx\"); }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      [InlineAsmWithx86Instructions, AsmBlocks]

testLintAsmBlocks :: Spec
testLintAsmBlocks =
  describe "lintAsmBlocks" $ do
    shouldLeaveUnresolved "leaves AsmBlocks unresolved"
      "void f() { __asm__(\"nop\"); }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      [AsmBlocks]

testLintHandleTypesCastToInt :: Spec
testLintHandleTypesCastToInt =
  describe "lintHandleTypesCastToInt" $ do
    shouldLintTo "rewrites (int)HANDLE to (intptr_t)HANDLE"
      "typedef void* HANDLE; int f() { HANDLE h = 0; return (int)h; }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      "intptr_t"
    shouldLintExactly "exact output for (int)HANDLE rewrite to intptr_t"
      "typedef void* HANDLE; int f() { HANDLE h = 0; return (int)h; }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      "typedef void * HANDLE; int f() { HANDLE h = 0; return (intptr_t) h; }"

testLintHandleTypesCastToUInt :: Spec
testLintHandleTypesCastToUInt =
  describe "lintHandleTypesCastToUInt" $ do
    shouldLintTo "rewrites (unsigned int)HANDLE to (uintptr_t)HANDLE"
      "typedef void* HANDLE; unsigned int f() { HANDLE h = 0; return (unsigned int)h; }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      "uintptr_t"
    shouldLintExactly "exact output for (unsigned int)HANDLE rewrite to uintptr_t"
      "typedef void* HANDLE; unsigned int f() { HANDLE h = 0; return (unsigned int)h; }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      "typedef void * HANDLE; unsigned int f() { HANDLE h = 0; return (uintptr_t) h; }"

testLintX86SpecificCompilerIntrinsics :: Spec
testLintX86SpecificCompilerIntrinsics =
  describe "lintX86SpecificCompilerIntrinsics" $ do
    shouldLeaveUnresolved "leaves X86SpecificCompilerIntrinsics unresolved"
      "void f() { _mm_pause(); }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      [X86SpecificCompilerIntrinsics]

testLintAssumptionsAboutRegSizes :: Spec
testLintAssumptionsAboutRegSizes =
  describe "lintAssumptionsAboutRegSizes" $ do
    shouldLeaveUnresolved "leaves AssumptionsAboutRegSizes unresolved"
      "void f() { if (sizeof(int) == 4) { } }"
      analyzePlatformSpecificIssues
      lintPlatformSpecificsIssues
      [AssumptionsAboutRegSizes]
