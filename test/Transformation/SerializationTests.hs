module Transformation.SerializationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Serialization
import Analysis.UtilTypes

serializationTransformSpec :: Spec
serializationTransformSpec = describe "Serialization Transformations" $ do
  testTransformWritingPtrDirectToFile
  testTransformWritingPtrContrainingStructsToFiles
  testTransformSendingPtrsOverNetwork
  testTransformPtrInMemoryMappedFiles
  testTransformPtrInSharedMemory

testTransformWritingPtrDirectToFile :: Spec
testTransformWritingPtrDirectToFile =
  describe "transformWritingPtrDirectToFile" $ do
    it "TODO: implement transformation tests" pending

testTransformWritingPtrContrainingStructsToFiles :: Spec
testTransformWritingPtrContrainingStructsToFiles =
  describe "transformWritingPtrContrainingStructsToFiles" $ do
    it "TODO: implement transformation tests" pending

testTransformSendingPtrsOverNetwork :: Spec
testTransformSendingPtrsOverNetwork =
  describe "transformSendingPtrsOverNetwork" $ do
    it "TODO: implement transformation tests" pending

testTransformPtrInMemoryMappedFiles :: Spec
testTransformPtrInMemoryMappedFiles =
  describe "transformPtrInMemoryMappedFiles" $ do
    it "TODO: implement transformation tests" pending

testTransformPtrInSharedMemory :: Spec
testTransformPtrInSharedMemory =
  describe "transformPtrInSharedMemory" $ do
    it "TODO: implement transformation tests" pending
