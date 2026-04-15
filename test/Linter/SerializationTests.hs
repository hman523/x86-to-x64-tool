module Linter.SerializationTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.Serialization
import Analysis.Serialization (analyzeSerializationIssues)
import Analysis.IssueTypes

serializationLintSpec :: Spec
serializationLintSpec = describe "Serialization Linting" $ do
  testLintWritingPtrDirectToFile
  testLintWritingPtrContrainingStructsToFiles
  testLintSendingPtrsOverNetwork
  testLintPtrInMemoryMappedFiles
  testLintPtrInSharedMemory

testLintWritingPtrDirectToFile :: Spec
testLintWritingPtrDirectToFile =
  describe "lintWritingPtrDirectToFile" $ do
    shouldLeaveUnresolved "leaves WritingPtrDirectToFile unresolved"
      "void f() { int *p = 0; fwrite(&p, sizeof(p), 1, 0); }"
      analyzeSerializationIssues
      lintSerializationIssues
      [WritingPtrDirectToFile]

testLintWritingPtrContrainingStructsToFiles :: Spec
testLintWritingPtrContrainingStructsToFiles =
  describe "lintWritingPtrContrainingStructsToFiles" $ do
    shouldLeaveUnresolved "leaves WritingPtrContrainingStructsToFiles unresolved"
      "struct S { int *p; }; void f() { struct S s; fwrite(&s, sizeof(s), 1, 0); }"
      analyzeSerializationIssues
      lintSerializationIssues
      [WritingPtrContrainingStructsToFiles]

testLintSendingPtrsOverNetwork :: Spec
testLintSendingPtrsOverNetwork =
  describe "lintSendingPtrsOverNetwork" $ do
    shouldLeaveUnresolved "leaves SendingPtrsOverNetwork unresolved"
      "void f() { int *p = 0; send(0, &p, sizeof(p), 0); }"
      analyzeSerializationIssues
      lintSerializationIssues
      [SendingPtrsOverNetwork]

testLintPtrInMemoryMappedFiles :: Spec
testLintPtrInMemoryMappedFiles =
  describe "lintPtrInMemoryMappedFiles" $ do
    shouldLeaveUnresolved "leaves PtrInMemoryMappedFiles unresolved"
      "void f() { int **pp = 0; pp = mmap(0, 4096, 0, 0, 0, 0); }"
      analyzeSerializationIssues
      lintSerializationIssues
      [PtrInMemoryMappedFiles]

testLintPtrInSharedMemory :: Spec
testLintPtrInSharedMemory =
  describe "lintPtrInSharedMemory" $ do
    shouldLeaveUnresolved "leaves PtrInSharedMemory unresolved"
      "void f() { shm_open(\"name\", 0, 0); }"
      analyzeSerializationIssues
      lintSerializationIssues
      [PtrInSharedMemory]
