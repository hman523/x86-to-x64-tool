module Linter.PointerMathTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.PointerMath
import Analysis.PointerMath (analyzePointerMathIssues)
import Analysis.IssueTypes
import Parser.Parser (parseSourceString)

pointerMathLintSpec :: Spec
pointerMathLintSpec = describe "PointerMath Linting" $ do
  testLintPtrDiffStoredAs32bit
  testLintPointerAddOverflow
  testLintPtrSubUnderflow
  testLintArrayIndexingIntInArrayOver2tothe31size
  testLintScopedPointerMath
  testLintScopedPointerMathEdgeCases
  testLintScopedVariableShadowing

testLintPtrDiffStoredAs32bit :: Spec
testLintPtrDiffStoredAs32bit =
  describe "lintPtrDiffStoredAs32bit" $ do
    shouldLintTo "rewrites int variable storing ptr diff to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; int d; d = p - q; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "ptrdiff_t"
    shouldLintExactly "exact output for int ptr-diff rewrite to ptrdiff_t"
      "void f() { int *p = 0; int *q = 0; int d; d = p - q; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "void f() { int * p = 0; int * q = 0; ptrdiff_t d; d = p - q; }"

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
    shouldLintExactly "exact output for int array index rewrite to ptrdiff_t"
      "void f() { int arr[10]; int i = 0; int x = arr[i]; }"
      analyzePointerMathIssues
      lintPointerMathIssues
      "void f() { int arr[10]; ptrdiff_t i = 0; int x = arr[i]; }"

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
    shouldLintExactly
        "exact output for ptr-diff in if-block rewrite to ptrdiff_t"
        "void f(int *p, int *q) { if (p) { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "void f(int * p, int * q) { if (p) { ptrdiff_t d; d = p - q; } }"

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
    shouldLintExactly
        "exact output for ptr-diff in switch case rewrite to ptrdiff_t"
        "void f(int *p, int *q, int n) { switch (n) { case 1: { int d; d = p - q; break; } } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "void f(int * p, int * q, int n) { switch (n) { case 1: { ptrdiff_t d; d = p - q; break; } } }"

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

-- | Tests verifying that same-named variables in different scopes are
--   analysed and linted independently (by NodeInfo, not by name alone).
testLintScopedVariableShadowing :: Spec
testLintScopedVariableShadowing =
  describe "scoped variable shadowing: independent linting by NodeInfo" $ do

    -- Only the if-branch 'int d' is assigned a ptr-diff: exactly one issue.
    it "only if-branch int d (ptr-diff) is flagged: exactly one issue found" $ do
      let code = "void f(int *p, int *q, int c) \
                 \{ if (c) { int d; d = p - q; } else { int d; d = 42; } }"
      case parseSourceString code of
        Left err  -> fail (show err)
        Right ast -> length (analyzePointerMathIssues ast) `shouldBe` 1

    -- Only the if-branch 'd' is retyped; the else-branch 'd' must stay 'int'.
    shouldLintTo
        "int d in if-branch (ptr-diff) is retyped to ptrdiff_t"
        "void f(int *p, int *q, int c) { if (c) { int d; d = p - q; } else { int d; d = 42; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Both branches have ptr-diff: both are fully resolved.
    shouldFullyLint
        "int d in both if and else branches (both ptr-diff): both retyped"
        "void f(int *p, int *q, int c) { if (c) { int d; d = p - q; } else { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues

    -- Outer 'int d' has no ptr-diff; inner same-named 'int d' does.
    it "only inner int d (ptr-diff) is flagged when outer int d has literal: one issue" $ do
      let code = "void f(int *p, int *q) { int d; d = 5; { int d; d = p - q; } }"
      case parseSourceString code of
        Left err  -> fail (show err)
        Right ast -> length (analyzePointerMathIssues ast) `shouldBe` 1

    shouldLintTo
        "inner int d (ptr-diff) retyped to ptrdiff_t; outer int d (literal) unchanged"
        "void f(int *p, int *q) { int d; d = 5; { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t"

    -- Three successive scoped blocks, each with a separate 'int d' and ptr-diff.
    shouldFullyLint
        "three int d variables in successive scoped blocks with ptr-diff: all retyped"
        "void f(int *p, int *q) \
        \{ { int d; d = p - q; } { int d; d = p - q; } { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues

    -- Array index 'int i' in outer scope and same-named inner block:
    -- analysis detects both usages via their own NodeInfos.
    shouldFullyLint
        "outer int i (array index) and inner same-named int i (array index): both issues resolved"
        "void f(int *arr) { int i = 0; arr[i]; { int i = 1; arr[i]; } }"
        analyzePointerMathIssues
        lintPointerMathIssues

    -- Same-named int d in if- and else-branches: only the if-branch (ptr-diff)
    -- is retyped. The else-branch literal value must remain plain 'int'.
    shouldLintNotTo
        "else-branch int d (literal) is not retyped to ptrdiff_t"
        "void f(int *p, int *q, int c) { if (c) { int d; d = p - q; } else { int d; d = 42; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
        "ptrdiff_t d; d = 42"

    -- Outer int d has a plain literal; inner same-named int d has a ptr-diff.
    -- Exactly one issue should be detected (the inner one).
    it "outer int d (literal) + inner int d (ptr-diff): exactly one issue detected" $ do
      let code = "void f(int *p, int *q) { int d = 42; { int d; d = p - q; } }"
      case parseSourceString code of
        Left err  -> fail (show err)
        Right ast -> length (analyzePointerMathIssues ast) `shouldBe` 1

    shouldFullyLint
        "int d with ptr-diff in for-loop body fully resolved"
        "void f(int *p, int *q, int n) { for (int i = 0; i < n; i++) { int d; d = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues

    shouldFullyLint
        "int i as array index in two separate functions: both issues resolved"
        "void f(int *arr) { int i = 0; int x = arr[i]; } \
        \void g(int *arr) { int i = 1; int y = arr[i]; }"
        analyzePointerMathIssues
        lintPointerMathIssues

    shouldFullyLint
        "outer int i (array index) and inner int i (ptr-diff): both issues resolved"
        "void f(int *arr, int *p, int *q) { int i = 0; arr[i]; { int i; i = p - q; } }"
        analyzePointerMathIssues
        lintPointerMathIssues
