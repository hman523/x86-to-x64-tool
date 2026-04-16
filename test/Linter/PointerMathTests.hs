module Linter.PointerMathTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.PointerMath
import Analysis.PointerMath (analyzePointerMathIssues)
import Analysis.IssueTypes

pointerMathLintSpec :: Spec
pointerMathLintSpec = describe "PointerMath Linting" $ do
  testLintPtrDiffStoredAs32bit
  testLintPointerAddOverflow
  testLintPtrSubUnderflow
  testLintArrayIndexingIntInArrayOver2tothe31size
  testLintScopedPointerMath
  testLintScopedPointerMathEdgeCases

testLintPtrDiffStoredAs32bit :: Spec
testLintPtrDiffStoredAs32bit =
  describe "lintPtrDiffStoredAs32bit" $ do
    shouldLintTo "rewrites int variable storing ptr diff to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; int d; d = p - q; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "ptrdiff_t"

testLintPointerAddOverflow :: Spec
testLintPointerAddOverflow =
  describe "lintPointerAddOverflow" $ do
    shouldLeaveUnresolved "leaves PointerAddOverflow unresolved"
      "void f() { int *p = 0; int n = 1; int *q = p + n; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      [PointerAddOverflow]

testLintPtrSubUnderflow :: Spec
testLintPtrSubUnderflow =
  describe "lintPtrSubUnderflow" $ do
    shouldLeaveUnresolved "leaves PtrSubUnderflow unresolved"
      "void f() { int *p = 0; unsigned int n = 1; int *q = p - n; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      [PtrSubUnderflow]

testLintArrayIndexingIntInArrayOver2tothe31size :: Spec
testLintArrayIndexingIntInArrayOver2tothe31size =
  describe "lintArrayIndexingIntInArrayOver2tothe31size" $ do
    shouldLintTo "rewrites int index variable to ptrdiff_t"
      "void f() { int arr[10]; int i = 0; int x = arr[i]; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "ptrdiff_t"

-- | Pointer-math linting should find and rewrite variables in nested scopes.
testLintScopedPointerMath :: Spec
testLintScopedPointerMath =
  describe "scoped pointer-math linting" $ do

    shouldLintTo
        "int storing ptr-diff inside if-block is rewritten to ptrdiff_t"
        "void f(int *p, int *q) { if (p) { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    shouldLintTo
        "int storing ptr-diff inside while body is rewritten to ptrdiff_t"
        "void f(int *p, int *q) { while (p) { int d; d = p - q; p++; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    shouldLintTo
        "int storing ptr-diff inside for-loop body is rewritten to ptrdiff_t"
        "void f(int *p, int *q, int n) { for (int i = 0; i < n; i++) { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    shouldLintTo
        "int array index inside nested block is rewritten to ptrdiff_t"
        "void f(int *arr) { { int i = 0; int x = arr[i]; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    shouldFullyLint
        "ptr-diff variable in if-block is fully resolved (no remaining issues)"
        "void f(int *p, int *q) { if (p) { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues

    shouldFullyLint
        "ptr-diff variable in two separate functions are both resolved"
        "void f(int *p, int *q) { int d1; d1 = p - q; } \
        \void g(int *a, int *b) { int d2; d2 = a - b; }"
        analyzePointerMathIssues
        lintPointerMathIssues

-- | Edge cases for scoped pointer-math linting: switch, deeply nested, and
--   multi-function scenarios.
testLintScopedPointerMathEdgeCases :: Spec
testLintScopedPointerMathEdgeCases =
  describe "scoped pointer-math linting (edge cases)" $ do

    -- Switch case
    shouldLintTo
        "int storing ptr-diff inside switch case is rewritten to ptrdiff_t"
        "void f(int *p, int *q, int n) { switch (n) { case 1: { int d; d = p - q; break; } } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Array index in switch case
    shouldLintTo
        "int array index inside switch case is rewritten to ptrdiff_t"
        "void f(int *arr, int n) { switch (n) { case 0: { int i = 0; int x = arr[i]; break; } } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Deeply nested: if inside while inside for
    shouldLintTo
        "int storing ptr-diff in if-inside-while-inside-for is rewritten to ptrdiff_t"
        "void f(int *p, int *q, int n) { \
        \for (int i = 0; i < n; i++) { \
        \  while (p) { \
        \    if (i > 0) { int d; d = p - q; } \
        \    break; } } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Variable declared in outer scope, assigned (ptr-diff) in inner scope
    shouldLintTo
        "int declared outer, assigned ptr-diff in if-branch, is rewritten to ptrdiff_t"
        "void f(int *p, int *q) { int d; if (p) { d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Three functions — all three variables must be rewritten
    shouldFullyLint
        "ptr-diff in three separate functions all fully resolved"
        "void f(int *p, int *q) { int d1; d1 = p - q; } \
        \void g(int *a, int *b) { int d2; d2 = a - b; } \
        \void h(int *x, int *y) { int d3; d3 = x - y; }"
        analyzePointerMathIssues
        lintPointerMathIssues

    -- One clean function, one dirty — clean result must contain ptrdiff_t
    shouldLintTo
        "ptr-diff in second function is rewritten even though first function is clean"
        "void clean(void) { int x = 5; } \
        \void dirty(int *p, int *q) { int d; d = p - q; }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"
