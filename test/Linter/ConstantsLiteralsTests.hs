module Linter.ConstantsLiteralsTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.ConstantsLiterals
import Analysis.ConstantsLiterals (analyzeConstantsLiteralsIssues)
import Analysis.IssueTypes

constantsLiteralsLintSpec :: Spec
constantsLiteralsLintSpec = describe "ConstantsLiterals Linting" $ do
  testLintMagicValuesUsed
  testLintBitMaskingAssuming32bitPts
  testLintHardCodedAddressValues
  testLintConstantsUsedForSizeCalcs

testLintMagicValuesUsed :: Spec
testLintMagicValuesUsed =
  describe "lintMagicValuesUsed" $ do
    shouldLeaveUnresolved "leaves MagicValuesUsed unresolved"
      "void f() { void *p = malloc(4); }"
      analyzeConstantsLiteralsIssues
      lintConstantsLiteralsIssues
      [MagicValuesUsed]

testLintBitMaskingAssuming32bitPts :: Spec
testLintBitMaskingAssuming32bitPts =
  describe "lintBitMaskingAssuming32bitPts" $ do
    shouldLeaveUnresolved "leaves BitMaskingAssuming32bitPts unresolved"
      "void f() { int *p = 0; int x = p & 0xFFFFFFFF; }"
      analyzeConstantsLiteralsIssues
      lintConstantsLiteralsIssues
      [BitMaskingAssuming32bitPts]

testLintHardCodedAddressValues :: Spec
testLintHardCodedAddressValues =
  describe "lintHardCodedAddressValues" $ do
    shouldLeaveUnresolved "leaves HardCodedAddressValues unresolved"
      "void f() { int *p = (int *)0xDEADBEEF; }"
      analyzeConstantsLiteralsIssues
      lintConstantsLiteralsIssues
      [HardCodedAddressValues]

testLintConstantsUsedForSizeCalcs :: Spec
testLintConstantsUsedForSizeCalcs =
  describe "lintConstantsUsedForSizeCalcs" $ do
    shouldLeaveUnresolved "leaves ConstantsUsedForSizeCalcs unresolved"
      "void f() { unsigned int n; n = 42; }"
      analyzeConstantsLiteralsIssues
      lintConstantsLiteralsIssues
      [ConstantsUsedForSizeCalcs]
