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
