module Transformation.PlatformSpecificsTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.PlatformSpecifics
import Analysis.IssueTypes

platformSpecificsTransformSpec :: Spec
platformSpecificsTransformSpec = describe "PlatformSpecifics Transformations" $ do
  testTransformInlineAsmWithx86Instructions
  testTransformAsmBlocks
  testTransformHandleTypesCastToInt
  testTransformX86SpecificCompilerIntrinsics
  testTransformAssumptionsAboutRegSizes

testTransformInlineAsmWithx86Instructions :: Spec
testTransformInlineAsmWithx86Instructions =
  describe "transformInlineAsmWithx86Instructions" $ do
    it "TODO: implement transformation tests" pending

testTransformAsmBlocks :: Spec
testTransformAsmBlocks =
  describe "transformAsmBlocks" $ do
    it "TODO: implement transformation tests" pending

testTransformHandleTypesCastToInt :: Spec
testTransformHandleTypesCastToInt =
  describe "transformHandleTypesCastToInt" $ do
    it "TODO: implement transformation tests" pending

testTransformX86SpecificCompilerIntrinsics :: Spec
testTransformX86SpecificCompilerIntrinsics =
  describe "transformX86SpecificCompilerIntrinsics" $ do
    it "TODO: implement transformation tests" pending

testTransformAssumptionsAboutRegSizes :: Spec
testTransformAssumptionsAboutRegSizes =
  describe "transformAssumptionsAboutRegSizes" $ do
    it "TODO: implement transformation tests" pending
