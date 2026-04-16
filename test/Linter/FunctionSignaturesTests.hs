module Linter.FunctionSignaturesTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.FunctionSignatures
import Analysis.FunctionSignatures (analyzeFunctionSignatureIssues)

functionSignaturesLintSpec :: Spec
functionSignaturesLintSpec = describe "FunctionSignatures Linting" $ do
  testLintFnsReturnPtrAsInt
  testLintFnsReturnPtrAsUInt
  testLintFnsReturnPtrAsLong
  testLintFnsParamDeclaredAsIntTakesPtr
  testLintFnsParamDeclaredAsUIntTakesPtr
  testLintVaargUsingWrongTypesForPtrArgs
  testLintVaargUsingWrongTypesForPtrArgsUInt

testLintFnsReturnPtrAsInt :: Spec
testLintFnsReturnPtrAsInt =
  describe "lintFnsReturnPtrAsInt" $ do
    shouldLintTo "wraps pointer return value in (intptr_t) cast"
      "int f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"

testLintFnsReturnPtrAsUInt :: Spec
testLintFnsReturnPtrAsUInt =
  describe "lintFnsReturnPtrAsUInt" $ do
    shouldLintTo "wraps pointer return value in (uintptr_t) cast for unsigned int return type"
      "unsigned int f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "uintptr_t"

testLintFnsReturnPtrAsLong :: Spec
testLintFnsReturnPtrAsLong =
  describe "lintFnsReturnPtrAsLong" $ do
    shouldLintTo "wraps pointer return value in (intptr_t) cast (long return type)"
      "long f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"

testLintFnsParamDeclaredAsIntTakesPtr :: Spec
testLintFnsParamDeclaredAsIntTakesPtr =
  describe "lintFnsParamDeclaredAsIntTakesPtr" $ do
    shouldLintTo "rewrites int parameter that receives pointer to intptr_t"
      "void f(int p) { int *q = 0; p = q; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"

testLintFnsParamDeclaredAsUIntTakesPtr :: Spec
testLintFnsParamDeclaredAsUIntTakesPtr =
  describe "lintFnsParamDeclaredAsUIntTakesPtr" $ do
    shouldLintTo "rewrites unsigned int parameter that receives pointer to uintptr_t"
      "void f(unsigned int p) { int *q = 0; p = q; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "uintptr_t"

testLintVaargUsingWrongTypesForPtrArgs :: Spec
testLintVaargUsingWrongTypesForPtrArgs =
  describe "lintVaargUsingWrongTypesForPtrArgs" $ do
    shouldLintTo "rewrites va_arg(ap, int) extracting a pointer to intptr_t"
      "void f(__builtin_va_list ap) { int x = __builtin_va_arg(ap, int); }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"

testLintVaargUsingWrongTypesForPtrArgsUInt :: Spec
testLintVaargUsingWrongTypesForPtrArgsUInt =
  describe "lintVaargUsingWrongTypesForPtrArgsUInt" $ do
    shouldLintTo "rewrites va_arg(ap, unsigned int) extracting a pointer to uintptr_t"
      "void f(__builtin_va_list ap) { unsigned int x = __builtin_va_arg(ap, unsigned int); }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "uintptr_t"
