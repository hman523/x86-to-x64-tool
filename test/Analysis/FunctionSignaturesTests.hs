module Analysis.FunctionSignaturesTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.FunctionSignatures
import Analysis.IssueTypes

functionSignaturesSpec :: Spec
functionSignaturesSpec = describe "FunctionSignatures Analysis" $ do

    describe "checkFnsReturnPtrAsInt" $ do
        shouldFlagError
            "flags function returning int that returns a local pointer variable"
            "int foo() { int *p; return p; }"
            checkFnsReturnPtrAsInt

        shouldFlagErrorWithDetails
            "flags function returning unsigned int that returns a pointer as FnsReturnPtrAsUInt"
            "unsigned int foo() { int *p; return p; }"
            checkFnsReturnPtrAsInt
            FnsReturnPtrAsUInt
            Nothing

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

        shouldFlagErrorWithDetails
            "flags param declared as unsigned int receiving a pointer as FnsParamDeclaredAsUIntTakesPtr"
            "void foo(unsigned int handle) { int *p; handle = p; }"
            checkFnsParamDeclaredAsIntTakesPtr
            FnsParamDeclaredAsUIntTakesPtr
            Nothing

        shouldNotFlagError
            "does not flag int param receiving int value"
            "void foo(int x) { int y = 5; x = y; }"
            checkFnsParamDeclaredAsIntTakesPtr

    describe "checkVaargUsingWrongTypesForPtrArgs" $ do
        shouldFlagError
            "flags va_arg extracting int (likely a pointer arg)"
            "void foo(int n, ...) { __builtin_va_list ap; int x = __builtin_va_arg(ap, int); }"
            checkVaargUsingWrongTypesForPtrArgs

        shouldFlagErrorWithDetails
            "flags va_arg extracting unsigned int as VaargUsingWrongTypesForPtrArgsUInt"
            "void foo(int n, ...) { __builtin_va_list ap; unsigned int x = __builtin_va_arg(ap, unsigned int); }"
            checkVaargUsingWrongTypesForPtrArgs
            VaargUsingWrongTypesForPtrArgsUInt
            Nothing

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

    describe "function signature edge cases" $ do

        shouldFlagNIssues
            "three int params all receiving different pointer types produce three issues"
            "void foo(int h1, int h2, int h3) { int *p; char *s; void *v; h1 = p; h2 = s; h3 = v; }"
            checkFnsParamDeclaredAsIntTakesPtr
            3

        shouldNotFlagError
            "function returning pointer declared as long* is not flagged by checkFnsReturnPtrAsInt"
            "long *foo() { long *p = 0; return p; }"
            checkFnsReturnPtrAsInt

        shouldFlagError
            "function returning void* declared as int is flagged"
            "int foo() { void *p = 0; return p; }"
            checkFnsReturnPtrAsInt

        shouldNotFlagError
            "function pointer declaration with long param does not trigger checkFnsReturnPtrAsLong (only definitions are checked)"
            "typedef long (*callback_t)(void *p);"
            checkFnsReturnPtrAsLong

    describe "function signature additional edge cases" $ do

        -- Two va_arg(ap, int) calls in the same function: both are suspicious
        shouldFlagNIssues
            "two va_arg(ap, int) calls in one function produce exactly two issues"
            "void foo(int n, ...) { __builtin_va_list ap; int a = __builtin_va_arg(ap, int); int b = __builtin_va_arg(ap, int); }"
            checkVaargUsingWrongTypesForPtrArgs
            2

        -- va_arg extracting the correct type (int*) should not trigger the check
        shouldNotFlagError
            "does not flag va_arg(ap, int*) — extracting the correct pointer type"
            "void foo(int n, ...) { __builtin_va_list ap; int *p = __builtin_va_arg(ap, int*); }"
            checkVaargUsingWrongTypesForPtrArgs

        -- Returning NULL (zero) from an int-typed function is fine
        shouldNotFlagError
            "does not flag returning 0 from int function (null/zero, not a pointer)"
            "int foo() { return 0; }"
            checkFnsReturnPtrAsInt

        -- A function declared long* is not a function returning ptr-as-int
        shouldNotFlagError
            "does not flag function correctly declared as returning long*"
            "long *alloc() { long *p = 0; return p; }"
            checkFnsReturnPtrAsInt
