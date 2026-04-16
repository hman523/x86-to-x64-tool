module Analysis.PointerMathTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.PointerMath
import Analysis.IssueTypes

pointerMathSpec :: Spec
pointerMathSpec = describe "PointerMath Analysis" $ do

    describe "checkPtrDiffStoredAs32bit" $ do
        shouldFlagError
            "flags pointer subtraction stored in int (assignment form)"
            "void foo() { int *a; int *b; int diff; diff = a - b; }"
            checkPtrDiffStoredAs32bit

        shouldNotFlagError
            "does not flag pointer diff stored in long"
            "void foo() { int *a; int *b; long diff; diff = a - b; }"
            checkPtrDiffStoredAs32bit

        shouldFlagError
            "flags pointer subtraction stored in unsigned int"
            "void foo() { int *a; int *b; unsigned int diff; diff = a - b; }"
            checkPtrDiffStoredAs32bit

        shouldNotFlagError
            "does not flag non-pointer subtraction stored in int"
            "void foo() { int a = 10; int b = 3; int diff; diff = a - b; }"
            checkPtrDiffStoredAs32bit

    describe "checkPtrAddOverflow" $ do
        shouldFlagError
            "flags pointer addition with a named int variable"
            "void foo() { int *p; int n; int *q = p + n; }"
            checkPtrAddOverflow

        shouldNotFlagError
            "does not flag pointer addition with a long"
            "void foo() { int *p; long n; int *q = p + n; }"
            checkPtrAddOverflow

        shouldFlagError
            "flags pointer addition with an unsigned int offset"
            "void foo() { int *p; unsigned int n; int *q = p + n; }"
            checkPtrAddOverflow

    describe "checkPtrSubUnderflow" $ do
        shouldFlagError
            "flags pointer subtraction with unsigned int"
            "void foo() { int *p; unsigned int n; int *q = p - n; }"
            checkPtrSubUnderflow

        shouldNotFlagError
            "does not flag pointer subtraction with ptrdiff_t (long)"
            "void foo() { int *p; long n; int *q = p - n; }"
            checkPtrSubUnderflow

    describe "checkArrayIndexingIntInArrayOver2tothe31size" $ do
        shouldFlagError
            "flags array indexing with int variable"
            "void foo() { int arr[10]; int i; int x = arr[i]; }"
            checkArrayIndexingIntInArrayOver2tothe31size

        shouldNotFlagError
            "does not flag array indexing with long variable"
            "void foo() { int arr[10]; long i; int x = arr[i]; }"
            checkArrayIndexingIntInArrayOver2tothe31size

        shouldFlagError
            "flags array indexing with unsigned int variable"
            "void foo() { int arr[10]; unsigned int i; int x = arr[i]; }"
            checkArrayIndexingIntInArrayOver2tothe31size

        shouldNotFlagError
            "does not flag array indexing with integer literal"
            "void foo() { int arr[10]; int x = arr[0]; }"
            checkArrayIndexingIntInArrayOver2tothe31size

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all four pointer-math checks fire together in one function"
            "void foo() { int *a; int *b; int diff; diff = a - b; int n; int *c = a + n; unsigned int m; int *d = a - m; int arr[10]; int i; int x = arr[i]; }"
            analyzePointerMathIssues
            [PtrDiffStoredAs32bit, PointerAddOverflow, PtrSubUnderflow, ArrayIndexingIntInArrayOver2tothe31size]

        shouldFlagNIssues
            "two pointer-diff-to-int assignments produce exactly two issues"
            "void foo() { int *a; int *b; int *c; int d1; int d2; d1 = a - b; d2 = b - c; }"
            checkPtrDiffStoredAs32bit
            2

        shouldFlagAtLeastNIssues
            "combined unsafe pointer operations produce at least four distinct issues"
            "void foo() { int *a; int *b; int diff; diff = a - b; int n; int *c = a + n; unsigned int m; int *d = a - m; int arr[10]; int idx; int x = arr[idx]; }"
            analyzePointerMathIssues
            4

    describe "multi-dimensional array index edge cases" $ do

        shouldFlagNIssues
            "arr[i][j] with both int indices produces two issues"
            "void foo() { int arr[10][10]; int i; int j; int x = arr[i][j]; }"
            checkArrayIndexingIntInArrayOver2tothe31size
            2

        shouldNotFlagError
            "arr[i][j] with both long indices is not flagged"
            "void foo() { int arr[10][10]; long i; long j; int x = arr[i][j]; }"
            checkArrayIndexingIntInArrayOver2tothe31size

        shouldFlagNIssues
            "arr[i][j] with first index long and second int: only second is flagged"
            "void foo() { int arr[10][10]; long i; int j; int x = arr[i][j]; }"
            checkArrayIndexingIntInArrayOver2tothe31size
            1

        shouldFlagNIssues
            "three-level arr[i][j][k] with all int indices produces three issues"
            "void foo() { int arr[4][4][4]; int i; int j; int k; int x = arr[i][j][k]; }"
            checkArrayIndexingIntInArrayOver2tothe31size
            3
