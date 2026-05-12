module Linter.TypeSizeTests where

import Test.Hspec
import Language.C.Pretty (pretty)
import Text.PrettyPrint (render)
import Parser.Parser (parseSourceString)
import Linter.LinterTestsUtils
import Linter.TypeSize
import Analysis.TypeSize (analyzeTypeSizeIssues)
import Analysis.IssueTypes

typeSizeLintSpec :: Spec
typeSizeLintSpec = describe "TypeSize Linting" $ do
  testLintCastPointerToInt
  testLintCastPointerToUInt
  testLintCastIntToPointer
  testLintCastLongToPointer
  testLintSizeOfIntIsVoid
  testLintSizeOfLongIsVoid
  testLintUsingIntAsSizet
  testLintUsingIntAsPtrdifft
  testLintUsingUIntAsMemSize
  testLintMultiple
  testLintScopedIssues
  testLintChainedCasts


testLintCastPointerToInt :: Spec
testLintCastPointerToInt =
  describe "lintCastPointerToInt" $ do
    shouldLintTo "rewrites (int)ptr to (intptr_t)ptr"
      "int main() { int *ptr = 0; int x = (int)ptr; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "intptr_t"
    shouldLintExactly "exact output for (int)ptr rewrite to intptr_t"
      "int main() { int *ptr = 0; int x = (int)ptr; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "int main() { int * ptr = 0; int x = (intptr_t) ptr; return 0; }"
    shouldLeaveUnresolved "leaves unresolvable sizeof issue alone when linting cast"
      "int main() { int *ptr = 0; int x = (int)ptr; if (sizeof(int) == sizeof(void*)) return 1; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      [SizeOfIntIsVoid]

testLintCastPointerToUInt :: Spec
testLintCastPointerToUInt =
  describe "lintCastPointerToUInt" $ do
    shouldLintTo "rewrites (unsigned int)ptr to (uintptr_t)ptr"
      "int main() { int *ptr = 0; unsigned int x = (unsigned int)ptr; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "uintptr_t"
    shouldLintExactly "exact output for (unsigned int)ptr rewrite to uintptr_t"
      "int main() { int *ptr = 0; unsigned int x = (unsigned int)ptr; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "int main() { int * ptr = 0; unsigned int x = (uintptr_t) ptr; return 0; }"

testLintCastIntToPointer :: Spec
testLintCastIntToPointer =
  describe "lintCastIntToPointer" $ do
    shouldLeaveUnresolved "leaves (int*)x unresolved (fix changes pointer to int)"
      "int main() { int x = 5; void *p = (int*)x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      [CastIntToPointer]

testLintCastLongToPointer :: Spec
testLintCastLongToPointer =
  describe "lintCastLongToPointer" $ do
    shouldLeaveUnresolved "leaves (int*)long unresolved (fix changes pointer to int)"
      "int main() { long x = 5; void *p = (int*)x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      [CastLongToPointer]

testLintSizeOfIntIsVoid :: Spec
testLintSizeOfIntIsVoid =
  describe "lintSizeOfIntIsVoid" $ do
    shouldLeaveUnresolved "leaves sizeof(int)==sizeof(void*) unresolved"
      "int f() { return sizeof(int) == sizeof(void*); }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      [SizeOfIntIsVoid]

testLintSizeOfLongIsVoid :: Spec
testLintSizeOfLongIsVoid =
  describe "lintSizeOfLongIsVoid" $ do
    shouldLeaveUnresolved "leaves sizeof(long)==sizeof(void*) unresolved"
      "int f() { return sizeof(long) == sizeof(void*); }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      [SizeOfLongIsVoid]

testLintUsingIntAsSizet :: Spec
testLintUsingIntAsSizet =
  describe "lintUsingIntAsSizet" $ do
    shouldLintTo "rewrites size variable declaration to size_t"
      "int main() { int x = 5; unsigned long size; size = x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "size_t"
    shouldLintExactly "exact output for unsigned long size variable rewrite to size_t"
      "int main() { int x = 5; unsigned long size; size = x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "int main() { int x = 5; size_t size; size = x; return 0; }"

testLintUsingIntAsPtrdifft :: Spec
testLintUsingIntAsPtrdifft =
  describe "lintUsingIntAsPtrdifft" $ do
    shouldLintTo "rewrites diff variable declaration to ptrdiff_t"
      "int main() { int x = 5; long diff; diff = x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "ptrdiff_t"
    shouldLintExactly "exact output for long diff variable rewrite to ptrdiff_t"
      "int main() { int x = 5; long diff; diff = x; return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "int main() { int x = 5; ptrdiff_t diff; diff = x; return 0; }"

testLintUsingUIntAsMemSize :: Spec
testLintUsingUIntAsMemSize =
  describe "lintUsingUIntAsMemSize" $ do
    shouldLintTo "rewrites unsigned int variable passed to malloc to size_t"
      "int main() { unsigned int size = 10; void *ptr = malloc(size); return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "size_t"
    shouldLintExactly "exact output for unsigned int malloc arg rewrite to size_t"
      "int main() { unsigned int size = 10; void *ptr = malloc(size); return 0; }"
      analyzeTypeSizeIssues
      lintTypeSizeIssues
      "int main() { size_t size = 10; void * ptr = malloc(size); return 0; }"

-- | A single C function that triggers all four resolvable TypeSize issue
-- kinds plus one unresolvable sizeof comparison.  The linter should
-- rewrite every resolvable cast/declaration and leave only SizeOfIntIsVoid.
testLintMultiple :: Spec
testLintMultiple =
  describe "multiple TypeSize linting in one snippet" $ do
    it "rewrites all resolvable issues and leaves sizeof comparison unresolved" $ do
      let code = unlines
            [ "int main() {"
            , "    int *ptr = 0;"
            , "    int a = (int)ptr;"               -- CastPointerToInt  -> intptr_t
            , "    unsigned int b = (unsigned int)ptr;" -- CastPointerToUInt -> uintptr_t
            , "    int n = 5;"
            , "    unsigned long size;"
            , "    size = n;"                        -- UsingIntAsSizet   -> size_t
            , "    long diff;"
            , "    diff = n;"                        -- UsingIntAsPtrdifft -> ptrdiff_t
            , "    if (sizeof(int) == sizeof(void*)) return 1;" -- SizeOfIntIsVoid (unresolvable)
            , "    return 0;"
            , "}"
            ]
      case parseSourceString code of
        Left err  -> fail (show err)
        Right ast -> do
          let issues = analyzeTypeSizeIssues ast
          length issues `shouldBe` 5
          let (ast', unresolved) = lintTypeSizeIssues ast issues
          let output = render (pretty ast')
          -- All four resolvable rewrites should appear in the output
          output `shouldContain` "intptr_t"
          output `shouldContain` "uintptr_t"
          output `shouldContain` "size_t"
          output `shouldContain` "ptrdiff_t"
          -- Only the sizeof comparison should remain unresolved
          length unresolved `shouldBe` 1

-- | Verify that the linter correctly rewrites issues that appear inside
--   nested control-flow blocks (if, while, for) rather than at the top level.
testLintScopedIssues :: Spec
testLintScopedIssues =
  describe "scoped TypeSize linting" $ do

    shouldFullyLint
        "(int)ptr inside if-block is fully linted to intptr_t"
        "void f(void *p) { if (1) { int x = (int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldFullyLint
        "(int)ptr inside else-block is fully linted"
        "void f(void *p) { if (0) { } else { int x = (int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldFullyLint
        "(int)ptr inside while body is fully linted"
        "void f(void *p) { while (1) { int x = (int)p; break; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldFullyLint
        "(int)ptr inside for-loop body is fully linted"
        "void f(void *p) { for (int i = 0; i < 1; i++) { int x = (int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldFullyLint
        "(unsigned int)ptr inside nested block is fully linted to uintptr_t"
        "void f(void *p) { { unsigned int x = (unsigned int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldLintTo
        "(int)ptr in if-block produces intptr_t in output"
        "void f(void *p) { if (1) { int x = (int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "intptr_t"
    shouldLintExactly
        "exact output for (int)ptr in if-block rewrite to intptr_t"
        "void f(void *p) { if (1) { int x = (int)p; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "void f(void * p) { if (1) { int x = (intptr_t) p; } }"

    shouldLeaveUnresolved
        "(int*)long in while body is left unresolved"
        "void f(void) { long x = 5; while (1) { void *p = (int*)x; break; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        [CastLongToPointer]

    shouldLintTo
        "int size-variable inside if-block is rewritten to size_t"
        "void f(int n) { if (n > 0) { unsigned long sz; sz = n; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "size_t"
    shouldLintExactly
        "exact output for unsigned long sz in if-block rewrite to size_t"
        "void f(int n) { if (n > 0) { unsigned long sz; sz = n; } }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "void f(int n) { if (n > 0) { size_t sz; sz = n; } }"

    shouldFullyLint
        "casts in two separate functions are both linted"
        "void f(void *p) { int a = (int)p; } \
        \void g(void *q) { int b = (int)q; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

-- | Verify that chained casts like @(int)(long)ptr@ are fully resolved:
--   analysis detects the issue and the linter collapses the chain.
testLintChainedCasts :: Spec
testLintChainedCasts =
  describe "chained cast linting" $ do

    shouldFullyLint
        "(int)(long)ptr is fully linted to (intptr_t)ptr"
        "void f(void *p) { int x = (int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldLintTo
        "(int)(long)ptr produces intptr_t in the output"
        "void f(void *p) { int x = (int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "intptr_t"
    shouldLintExactly
        "exact output for (int)(long)ptr chain collapsed to (intptr_t)p"
        "void f(void *p) { int x = (int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "void f(void * p) { int x = (intptr_t) p; }"

    shouldLintNotTo
        "(int)(long)ptr: intermediate (long) cast is collapsed and absent"
        "void f(void *p) { int x = (int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "(long)"

    shouldFullyLint
        "(unsigned int)(long)ptr is fully linted to (uintptr_t)ptr"
        "void f(void *p) { unsigned int x = (unsigned int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues

    shouldLintTo
        "(unsigned int)(long)ptr produces uintptr_t in the output"
        "void f(void *p) { unsigned int x = (unsigned int)(long)p; }"
        analyzeTypeSizeIssues
        lintTypeSizeIssues
        "uintptr_t"
