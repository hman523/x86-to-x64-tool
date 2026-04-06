module Transformation.PlatformSpecificsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.PlatformSpecifics
import Analysis.PlatformSpecifics (analyzePlatformSpecificIssues)
import Analysis.IssueTypes

platformSpecificsTransformSpec :: Spec
platformSpecificsTransformSpec = describe "PlatformSpecifics Transformations" $ do
  testTransformInlineAsmWithx86Instructions
  testTransformAsmBlocks
  testTransformHandleTypesCastToInt
  testTransformX86SpecificCompilerIntrinsics
  testTransformAssumptionsAboutRegSizes

testTransformInlineAsmWithx86Instructions :: Spec
testTransformInlineAsmWithx86Instructions =
  describe "transformInlineAsmWithx86Instructions" $ do
    shouldLeaveUnresolved "leaves InlineAsmWithx86Instructions unresolved"
      "void f() { __asm__(\"movl %eax, %ebx\"); }"
      analyzePlatformSpecificIssues
      transformPlatformSpecificsIssues
      [InlineAsmWithx86Instructions, AsmBlocks]

testTransformAsmBlocks :: Spec
testTransformAsmBlocks =
  describe "transformAsmBlocks" $ do
    shouldLeaveUnresolved "leaves AsmBlocks unresolved"
      "void f() { __asm__(\"nop\"); }"
      analyzePlatformSpecificIssues
      transformPlatformSpecificsIssues
      [AsmBlocks]

testTransformHandleTypesCastToInt :: Spec
testTransformHandleTypesCastToInt =
  describe "transformHandleTypesCastToInt" $ do
    shouldTransformTo "rewrites (int)HANDLE to (intptr_t)HANDLE"
      "typedef void* HANDLE; int f() { HANDLE h = 0; return (int)h; }"
      analyzePlatformSpecificIssues
      transformPlatformSpecificsIssues
      "intptr_t"

testTransformX86SpecificCompilerIntrinsics :: Spec
testTransformX86SpecificCompilerIntrinsics =
  describe "transformX86SpecificCompilerIntrinsics" $ do
    shouldLeaveUnresolved "leaves X86SpecificCompilerIntrinsics unresolved"
      "void f() { _mm_pause(); }"
      analyzePlatformSpecificIssues
      transformPlatformSpecificsIssues
      [X86SpecificCompilerIntrinsics]

testTransformAssumptionsAboutRegSizes :: Spec
testTransformAssumptionsAboutRegSizes =
  describe "transformAssumptionsAboutRegSizes" $ do
    shouldLeaveUnresolved "leaves AssumptionsAboutRegSizes unresolved"
      "void f() { if (sizeof(int) == 4) { } }"
      analyzePlatformSpecificIssues
      transformPlatformSpecificsIssues
      [AssumptionsAboutRegSizes]
