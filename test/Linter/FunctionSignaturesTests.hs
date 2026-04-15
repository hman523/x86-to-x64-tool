module Linter.FunctionSignaturesTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.FunctionSignatures
import Analysis.FunctionSignatures (analyzeFunctionSignatureIssues)

functionSignaturesLintSpec :: Spec
functionSignaturesLintSpec = describe "FunctionSignatures Linting" $ do
  testLintFnsReturnPtrAsInt
  testLintFnsReturnPtrAsLong
  testLintFnsParamDeclaredAsIntTakesPtr
  testLintVaargUsingWrongTypesForPtrArgs

testLintFnsReturnPtrAsInt :: Spec
testLintFnsReturnPtrAsInt =
  describe "lintFnsReturnPtrAsInt" $ do
    shouldLintTo "wraps pointer return value in (intptr_t) cast"
      "int f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"

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

testLintVaargUsingWrongTypesForPtrArgs :: Spec
testLintVaargUsingWrongTypesForPtrArgs =
  describe "lintVaargUsingWrongTypesForPtrArgs" $ do
    shouldLintTo "rewrites va_arg(ap, int) extracting a pointer to intptr_t"
      "void f(__builtin_va_list ap) { int x = __builtin_va_arg(ap, int); }"
      analyzeFunctionSignatureIssues
      lintFunctionSignaturesIssues
      "intptr_t"
