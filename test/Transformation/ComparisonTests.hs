module Transformation.ComparisonTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Comparison
import Analysis.UtilTypes

comparisonTransformSpec :: Spec
comparisonTransformSpec = describe "Comparison Transformations" $ do
  testTransformLoopCounterAsIntWhenIteratingOverPtrArrays
  testTransformPtrComparisonWithIntConsts
  testTransformUsingIntForFileOffsets

testTransformLoopCounterAsIntWhenIteratingOverPtrArrays :: Spec
testTransformLoopCounterAsIntWhenIteratingOverPtrArrays =
  describe "transformLoopCounterAsIntWhenIteratingOverPtrArrays" $ do
    it "TODO: implement transformation tests" pending

testTransformPtrComparisonWithIntConsts :: Spec
testTransformPtrComparisonWithIntConsts =
  describe "transformPtrComparisonWithIntConsts" $ do
    it "TODO: implement transformation tests" pending

testTransformUsingIntForFileOffsets :: Spec
testTransformUsingIntForFileOffsets =
  describe "transformUsingIntForFileOffsets" $ do
    it "TODO: implement transformation tests" pending
