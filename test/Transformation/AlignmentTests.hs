module Transformation.AlignmentTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Alignment
import Analysis.IssueTypes

alignmentTransformSpec :: Spec
alignmentTransformSpec = describe "Alignment Transformations" $ do
  testTransformStructContainingPtrWrittenToBinFile
  testTransformStrucContainingPtrReadFromBinFile
  testTransformStructsWithMixedPtrNonPtrMembers
  testTransformUnionsContainingPtrAndInts
  testTransformPackedStructsWithPtrs
  testTransformSizeofStoredin32bits
  testTransformHardCodedStructSizes

testTransformStructContainingPtrWrittenToBinFile :: Spec
testTransformStructContainingPtrWrittenToBinFile =
  describe "transformStructContainingPtrWrittenToBinFile" $ do
    it "TODO: implement transformation tests" pending

testTransformStrucContainingPtrReadFromBinFile :: Spec
testTransformStrucContainingPtrReadFromBinFile =
  describe "transformStrucContainingPtrReadFromBinFile" $ do
    it "TODO: implement transformation tests" pending

testTransformStructsWithMixedPtrNonPtrMembers :: Spec
testTransformStructsWithMixedPtrNonPtrMembers =
  describe "transformStructsWithMixedPtrNonPtrMembers" $ do
    it "TODO: implement transformation tests" pending

testTransformUnionsContainingPtrAndInts :: Spec
testTransformUnionsContainingPtrAndInts =
  describe "transformUnionsContainingPtrAndInts" $ do
    it "TODO: implement transformation tests" pending

testTransformPackedStructsWithPtrs :: Spec
testTransformPackedStructsWithPtrs =
  describe "transformPackedStructsWithPtrs" $ do
    it "TODO: implement transformation tests" pending

testTransformSizeofStoredin32bits :: Spec
testTransformSizeofStoredin32bits =
  describe "transformSizeofStoredin32bits" $ do
    it "TODO: implement transformation tests" pending

testTransformHardCodedStructSizes :: Spec
testTransformHardCodedStructSizes =
  describe "transformHardCodedStructSizes" $ do
    it "TODO: implement transformation tests" pending
