module Analysis.FunctionSignaturesTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.FunctionSignatures
import Analysis.UtilTypes

functionSignaturesSpec :: Spec
functionSignaturesSpec = describe "FunctionSignatures Analysis" $ do

    describe "checkFnsReturnPtrAsInt" $ do
        shouldFlagError
            "flags function returning int that returns a local pointer variable"
            "int foo() { int *p; return p; }"
            checkFnsReturnPtrAsInt

        shouldNotFlagError
            "does not flag function that legitimately returns int"
            "int foo() { int x = 42; return x; }"
            checkFnsReturnPtrAsInt

        shouldNotFlagError
            "does not flag void function"
            "void foo() { return; }"
            checkFnsReturnPtrAsInt

    describe "checkFnsReturnPtrAsLong" $ do
        shouldFlagError
            "flags function returning long that returns a local pointer variable"
            "long foo() { int *p; return p; }"
            checkFnsReturnPtrAsLong

        shouldNotFlagError
            "does not flag function that legitimately returns long"
            "long foo() { long x = 100L; return x; }"
            checkFnsReturnPtrAsLong

    describe "checkFnsParamDeclaredAsIntTakesPtr" $ do
        shouldFlagError
            "flags param declared as int receiving a pointer value"
            "void foo(int handle) { int *p; handle = p; }"
            checkFnsParamDeclaredAsIntTakesPtr

        shouldNotFlagError
            "does not flag int param receiving int value"
            "void foo(int x) { int y = 5; x = y; }"
            checkFnsParamDeclaredAsIntTakesPtr

    describe "checkVaargUsingWrongTypesForPtrArgs" $ do
        shouldFlagError
            "flags va_arg extracting int (likely a pointer arg)"
            "void foo(int n, ...) { __builtin_va_list ap; int x = __builtin_va_arg(ap, int); }"
            checkVaargUsingWrongTypesForPtrArgs

        shouldNotFlagError
            "does not flag va_arg extracting a pointer type"
            "void foo(int n, ...) { __builtin_va_list ap; void *p = __builtin_va_arg(ap, void*); }"
            checkVaargUsingWrongTypesForPtrArgs
