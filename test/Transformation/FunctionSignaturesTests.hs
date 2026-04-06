module Transformation.FunctionSignaturesTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.FunctionSignatures
import Analysis.FunctionSignatures (analyzeFunctionSignatureIssues)

functionSignaturesTransformSpec :: Spec
functionSignaturesTransformSpec = describe "FunctionSignatures Transformations" $ do
  testTransformFnsReturnPtrAsInt
  testTransformFnsReturnPtrAsLong
  testTransformFnsParamDeclaredAsIntTakesPtr
  testTransformVaargUsingWrongTypesForPtrArgs

testTransformFnsReturnPtrAsInt :: Spec
testTransformFnsReturnPtrAsInt =
  describe "transformFnsReturnPtrAsInt" $ do
    shouldTransformTo "wraps pointer return value in (intptr_t) cast"
      "int f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      transformFunctionSignaturesIssues
      "intptr_t"

testTransformFnsReturnPtrAsLong :: Spec
testTransformFnsReturnPtrAsLong =
  describe "transformFnsReturnPtrAsLong" $ do
    shouldTransformTo "wraps pointer return value in (intptr_t) cast (long return type)"
      "long f() { int *ptr = 0; return ptr; }"
      analyzeFunctionSignatureIssues
      transformFunctionSignaturesIssues
      "intptr_t"

testTransformFnsParamDeclaredAsIntTakesPtr :: Spec
testTransformFnsParamDeclaredAsIntTakesPtr =
  describe "transformFnsParamDeclaredAsIntTakesPtr" $ do
    shouldTransformTo "rewrites int parameter that receives pointer to intptr_t"
      "void f(int p) { int *q = 0; p = q; }"
      analyzeFunctionSignatureIssues
      transformFunctionSignaturesIssues
      "intptr_t"

testTransformVaargUsingWrongTypesForPtrArgs :: Spec
testTransformVaargUsingWrongTypesForPtrArgs =
  describe "transformVaargUsingWrongTypesForPtrArgs" $ do
    shouldTransformTo "rewrites va_arg(ap, int) extracting a pointer to intptr_t"
      "void f(__builtin_va_list ap) { int x = __builtin_va_arg(ap, int); }"
      analyzeFunctionSignatureIssues
      transformFunctionSignaturesIssues
      "intptr_t"
