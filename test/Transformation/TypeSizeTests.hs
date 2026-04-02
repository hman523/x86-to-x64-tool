module Transformation.TypeSizeTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.TypeSize
import Analysis.IssueTypes

typeSizeTransformSpec :: Spec
typeSizeTransformSpec = describe "TypeSize Transformations" $ do
  testTransformCastPointerToInt
  testTransformCastPointerToUInt
  testTransformCastIntToPointer
  testTransformCastLongToPointer
  testTransformSizeOfIntIsVoid
  testTransformSizeOfLongIsVoid
  testTransformUsingIntAsSizet
  testTransformUsingIntAsPtrdifft
  testTransformUsingUIntAsMemSize

testTransformCastPointerToInt :: Spec
testTransformCastPointerToInt =
  describe "transformCastPointerToInt" $ do
    it "TODO: implement transformation tests" pending

testTransformCastPointerToUInt :: Spec
testTransformCastPointerToUInt =
  describe "transformCastPointerToUInt" $ do
    it "TODO: implement transformation tests" pending

testTransformCastIntToPointer :: Spec
testTransformCastIntToPointer =
  describe "transformCastIntToPointer" $ do
    it "TODO: implement transformation tests" pending

testTransformCastLongToPointer :: Spec
testTransformCastLongToPointer =
  describe "transformCastLongToPointer" $ do
    it "TODO: implement transformation tests" pending

testTransformSizeOfIntIsVoid :: Spec
testTransformSizeOfIntIsVoid =
  describe "transformSizeOfIntIsVoid" $ do
    it "TODO: implement transformation tests" pending

testTransformSizeOfLongIsVoid :: Spec
testTransformSizeOfLongIsVoid =
  describe "transformSizeOfLongIsVoid" $ do
    it "TODO: implement transformation tests" pending

testTransformUsingIntAsSizet :: Spec
testTransformUsingIntAsSizet =
  describe "transformUsingIntAsSizet" $ do
    it "TODO: implement transformation tests" pending

testTransformUsingIntAsPtrdifft :: Spec
testTransformUsingIntAsPtrdifft =
  describe "transformUsingIntAsPtrdifft" $ do
    it "TODO: implement transformation tests" pending

testTransformUsingUIntAsMemSize :: Spec
testTransformUsingUIntAsMemSize =
  describe "transformUsingUIntAsMemSize" $ do
    it "TODO: implement transformation tests" pending
