module Transformation.PointerMathTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.PointerMath
import Analysis.IssueTypes

pointerMathTransformSpec :: Spec
pointerMathTransformSpec = describe "PointerMath Transformations" $ do
  testTransformPtrDiffStoredAs32bit
  testTransformPointerAddOverflow
  testTransformPtrSubUnderflow
  testTransformArrayIndexingIntInArrayOver2tothe31size

testTransformPtrDiffStoredAs32bit :: Spec
testTransformPtrDiffStoredAs32bit =
  describe "transformPtrDiffStoredAs32bit" $ do
    it "TODO: implement transformation tests" pending

testTransformPointerAddOverflow :: Spec
testTransformPointerAddOverflow =
  describe "transformPointerAddOverflow" $ do
    it "TODO: implement transformation tests" pending

testTransformPtrSubUnderflow :: Spec
testTransformPtrSubUnderflow =
  describe "transformPtrSubUnderflow" $ do
    it "TODO: implement transformation tests" pending

testTransformArrayIndexingIntInArrayOver2tothe31size :: Spec
testTransformArrayIndexingIntInArrayOver2tothe31size =
  describe "transformArrayIndexingIntInArrayOver2tothe31size" $ do
    it "TODO: implement transformation tests" pending
