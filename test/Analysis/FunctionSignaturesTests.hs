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

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all four function-signature checks fire in one translation unit"
            "int a() { int *p; return p; } long b() { int *q; return q; } void c(int h) { int *r; h = r; } void d(int n, ...) { __builtin_va_list ap; int x = __builtin_va_arg(ap, int); }"
            analyzeFunctionSignatureIssues
            [FnsReturnPtrAsInt, FnsReturnPtrAsLong, FnsParamDeclaredAsIntTakesPtr, VaargUsingWrongTypesForPtrArgs]

        shouldFlagNIssues
            "two functions each returning pointer as int produces exactly two issues"
            "int a() { int *p; return p; } int b() { int *q; return q; }"
            analyzeFunctionSignatureIssues
            2

        shouldFlagNIssues
            "two int params both assigned a pointer each produce one issue"
            "void foo(int x, int y) { int *p; int *q; x = p; y = q; }"
            analyzeFunctionSignatureIssues
            2
