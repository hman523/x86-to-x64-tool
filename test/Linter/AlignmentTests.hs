module Linter.AlignmentTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.Alignment
import Analysis.Alignment (analyzeAlignmentIssues)
import Analysis.IssueTypes

alignmentLintSpec :: Spec
alignmentLintSpec = describe "Alignment Linting" $ do
  testLintStructContainingPtrWrittenToBinFile
  testLintStrucContainingPtrReadFromBinFile
  testLintStructsWithMixedPtrNonPtrMembers
  testLintUnionsContainingPtrAndInts
  testLintPackedStructsWithPtrs
  testLintSizeofStoredin32bits
  testLintHardCodedStructSizes

testLintStructContainingPtrWrittenToBinFile :: Spec
testLintStructContainingPtrWrittenToBinFile =
  describe "lintStructContainingPtrWrittenToBinFile" $ do
    shouldLeaveUnresolved "leaves StructContainingPtrWrittenToBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fwrite(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StructContainingPtrWrittenToBinFile]

testLintStrucContainingPtrReadFromBinFile :: Spec
testLintStrucContainingPtrReadFromBinFile =
  describe "lintStrucContainingPtrReadFromBinFile" $ do
    shouldLeaveUnresolved "leaves StrucContainingPtrReadFromBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fread(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StrucContainingPtrReadFromBinFile]

testLintStructsWithMixedPtrNonPtrMembers :: Spec
testLintStructsWithMixedPtrNonPtrMembers =
  describe "lintStructsWithMixedPtrNonPtrMembers" $ do
    shouldLeaveUnresolved "leaves StructsWithMixedPtrNonPtrMembers unresolved"
      "struct S { int *p; int x; };"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StructsWithMixedPtrNonPtrMembers]

testLintUnionsContainingPtrAndInts :: Spec
testLintUnionsContainingPtrAndInts =
  describe "lintUnionsContainingPtrAndInts" $ do
    shouldLeaveUnresolved "leaves UnionsContainingPtrAndInts unresolved"
      "union U { int *p; int x; };"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [UnionsContainingPtrAndInts]

testLintPackedStructsWithPtrs :: Spec
testLintPackedStructsWithPtrs =
  describe "lintPackedStructsWithPtrs" $ do
    shouldLeaveUnresolved "leaves PackedStructsWithPtrs unresolved"
      "struct __attribute__((packed)) S { int *p; void *q; };"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [PackedStructsWithPtrs]

testLintSizeofStoredin32bits :: Spec
testLintSizeofStoredin32bits =
  describe "lintSizeofStoredin32bits" $ do
    shouldLintTo "rewrites int variable storing sizeof result to size_t"
      "void f() { int n; n = sizeof(int); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      "size_t"

testLintHardCodedStructSizes :: Spec
testLintHardCodedStructSizes =
  describe "lintHardCodedStructSizes" $ do
    shouldLeaveUnresolved "leaves HardCodedStructSizes unresolved"
      "void f() { void *p = malloc(16); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [HardCodedStructSizes]
