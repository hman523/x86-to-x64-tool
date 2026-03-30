module Transformation.FormatStringsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.FormatStrings
import Analysis.UtilTypes

formatStringsTransformSpec :: Spec
formatStringsTransformSpec = describe "FormatStrings Transformations" $ do
  testTransformDUsedWithSizet
  testTransformUUsedWithSizet
  testTransformXUsedWithSizet
  testTransformDUsedWithPtrdifft
  testTransformUUsedWithPtrdifft
  testTransformDUsedWithPtr
  testTransformUUsedWithPtr
  testTransformXUsedWithPtr
  testTransformLuUsedForPtrSizedVals
  testTransformLdUsedWithLongAssuming64bits

testTransformDUsedWithSizet :: Spec
testTransformDUsedWithSizet =
  describe "transformDUsedWithSizet" $ do
    it "TODO: implement transformation tests" pending

testTransformUUsedWithSizet :: Spec
testTransformUUsedWithSizet =
  describe "transformUUsedWithSizet" $ do
    it "TODO: implement transformation tests" pending

testTransformXUsedWithSizet :: Spec
testTransformXUsedWithSizet =
  describe "transformXUsedWithSizet" $ do
    it "TODO: implement transformation tests" pending

testTransformDUsedWithPtrdifft :: Spec
testTransformDUsedWithPtrdifft =
  describe "transformDUsedWithPtrdifft" $ do
    it "TODO: implement transformation tests" pending

testTransformUUsedWithPtrdifft :: Spec
testTransformUUsedWithPtrdifft =
  describe "transformUUsedWithPtrdifft" $ do
    it "TODO: implement transformation tests" pending

testTransformDUsedWithPtr :: Spec
testTransformDUsedWithPtr =
  describe "transformDUsedWithPtr" $ do
    it "TODO: implement transformation tests" pending

testTransformUUsedWithPtr :: Spec
testTransformUUsedWithPtr =
  describe "transformUUsedWithPtr" $ do
    it "TODO: implement transformation tests" pending

testTransformXUsedWithPtr :: Spec
testTransformXUsedWithPtr =
  describe "transformXUsedWithPtr" $ do
    it "TODO: implement transformation tests" pending

testTransformLuUsedForPtrSizedVals :: Spec
testTransformLuUsedForPtrSizedVals =
  describe "transformLuUsedForPtrSizedVals" $ do
    it "TODO: implement transformation tests" pending

testTransformLdUsedWithLongAssuming64bits :: Spec
testTransformLdUsedWithLongAssuming64bits =
  describe "transformLdUsedWithLongAssuming64bits" $ do
    it "TODO: implement transformation tests" pending
