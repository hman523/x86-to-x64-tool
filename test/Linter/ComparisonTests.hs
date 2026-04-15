module Linter.ComparisonTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.Comparison
import Analysis.Comparison (analyzeComparisonIssues)
import Analysis.IssueTypes

comparisonLintSpec :: Spec
comparisonLintSpec = describe "Comparison Linting" $ do
  testLintLoopCounterAsIntWhenIteratingOverPtrArrays
  testLintPtrComparisonWithIntConsts
  testLintUsingIntForFileOffsets

testLintLoopCounterAsIntWhenIteratingOverPtrArrays :: Spec
testLintLoopCounterAsIntWhenIteratingOverPtrArrays =
  describe "lintLoopCounterAsIntWhenIteratingOverPtrArrays" $ do
    shouldLintTo "rewrites int loop counter to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; for (int i = 0; i < (p - q); i++) { } }"
      analyzeComparisonIssues
      lintComparisonIssues
      "ptrdiff_t"

testLintPtrComparisonWithIntConsts :: Spec
testLintPtrComparisonWithIntConsts =
  describe "lintPtrComparisonWithIntConsts" $ do
    shouldLeaveUnresolved "leaves PtrComparisonWithIntConsts unresolved"
      "void f() { int *p = 0; if (p > 1) { } }"
      analyzeComparisonIssues
      lintComparisonIssues
      [PtrComparisonWithIntConsts]

testLintUsingIntForFileOffsets :: Spec
testLintUsingIntForFileOffsets =
  describe "lintUsingIntForFileOffsets" $ do
    shouldLintTo "rewrites int offset variable passed to fseek to off_t"
      "void f() { int offset = 0; fseek(0, offset, 0); }"
      analyzeComparisonIssues
      lintComparisonIssues
      "off_t"
