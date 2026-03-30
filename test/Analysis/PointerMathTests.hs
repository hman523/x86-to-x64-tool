module Analysis.PointerMathTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.PointerMath
import Analysis.UtilTypes

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
