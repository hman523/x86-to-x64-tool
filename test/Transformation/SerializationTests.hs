module Transformation.SerializationTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Serialization
import Analysis.Serialization (analyzeSerializationIssues)
import Analysis.IssueTypes

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
    shouldLeaveUnresolved "leaves WritingPtrDirectToFile unresolved"
      "void f() { int *p = 0; fwrite(&p, sizeof(p), 1, 0); }"
      analyzeSerializationIssues
      transformSerializationIssues
      [WritingPtrDirectToFile]

testTransformWritingPtrContrainingStructsToFiles :: Spec
testTransformWritingPtrContrainingStructsToFiles =
  describe "transformWritingPtrContrainingStructsToFiles" $ do
    shouldLeaveUnresolved "leaves WritingPtrContrainingStructsToFiles unresolved"
      "struct S { int *p; }; void f() { struct S s; fwrite(&s, sizeof(s), 1, 0); }"
      analyzeSerializationIssues
      transformSerializationIssues
      [WritingPtrContrainingStructsToFiles]

testTransformSendingPtrsOverNetwork :: Spec
testTransformSendingPtrsOverNetwork =
  describe "transformSendingPtrsOverNetwork" $ do
    shouldLeaveUnresolved "leaves SendingPtrsOverNetwork unresolved"
      "void f() { int *p = 0; send(0, &p, sizeof(p), 0); }"
      analyzeSerializationIssues
      transformSerializationIssues
      [SendingPtrsOverNetwork]

testTransformPtrInMemoryMappedFiles :: Spec
testTransformPtrInMemoryMappedFiles =
  describe "transformPtrInMemoryMappedFiles" $ do
    shouldLeaveUnresolved "leaves PtrInMemoryMappedFiles unresolved"
      "void f() { int **pp = 0; pp = mmap(0, 4096, 0, 0, 0, 0); }"
      analyzeSerializationIssues
      transformSerializationIssues
      [PtrInMemoryMappedFiles]

testTransformPtrInSharedMemory :: Spec
testTransformPtrInSharedMemory =
  describe "transformPtrInSharedMemory" $ do
    shouldLeaveUnresolved "leaves PtrInSharedMemory unresolved"
      "void f() { shm_open(\"name\", 0, 0); }"
      analyzeSerializationIssues
      transformSerializationIssues
      [PtrInSharedMemory]
