module Transformation.FunctionSignaturesTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.FunctionSignatures
import Analysis.IssueTypes

functionSignaturesTransformSpec :: Spec
functionSignaturesTransformSpec = describe "FunctionSignatures Transformations" $ do
  testTransformFnsReturnPtrAsInt
  testTransformFnsReturnPtrAsLong
  testTransformFnsParamDeclaredAsIntTakesPtr
  testTransformVaargUsingWrongTypesForPtrArgs

testTransformFnsReturnPtrAsInt :: Spec
testTransformFnsReturnPtrAsInt =
  describe "transformFnsReturnPtrAsInt" $ do
    it "TODO: implement transformation tests" pending

testTransformFnsReturnPtrAsLong :: Spec
testTransformFnsReturnPtrAsLong =
  describe "transformFnsReturnPtrAsLong" $ do
    it "TODO: implement transformation tests" pending

testTransformFnsParamDeclaredAsIntTakesPtr :: Spec
testTransformFnsParamDeclaredAsIntTakesPtr =
  describe "transformFnsParamDeclaredAsIntTakesPtr" $ do
    it "TODO: implement transformation tests" pending

testTransformVaargUsingWrongTypesForPtrArgs :: Spec
testTransformVaargUsingWrongTypesForPtrArgs =
  describe "transformVaargUsingWrongTypesForPtrArgs" $ do
    it "TODO: implement transformation tests" pending
