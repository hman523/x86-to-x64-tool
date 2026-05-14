module Linter.AlignmentTests where

import Test.Hspec
import Linter.LinterTestsUtils
import Linter.Alignment
import Analysis.Alignment (analyzeAlignmentIssues)
import Analysis.IssueTypes

alignmentLintSpec :: Spec
alignmentLintSpec = describe "Alignment Linting" $ do
  testLintStructContainingPtrWrittenToBinFile
  testLintStructContainingPtrReadFromBinFile
  testLintStructsWithMixedPtrNonPtrMembers
  testLintUnionsContainingPtrAndInts
  testLintPackedStructsWithPtrs
  testLintSizeofStoredIn32Bits
  testLintHardCodedStructSizes

testLintStructContainingPtrWrittenToBinFile :: Spec
testLintStructContainingPtrWrittenToBinFile =
  describe "lintStructContainingPtrWrittenToBinFile" $ do
    shouldLeaveUnresolved "leaves StructContainingPtrWrittenToBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fwrite(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StructContainingPtrWrittenToBinFile]

testLintStructContainingPtrReadFromBinFile :: Spec
testLintStructContainingPtrReadFromBinFile =
  describe "lintStructContainingPtrReadFromBinFile" $ do
    shouldLeaveUnresolved "leaves StructContainingPtrReadFromBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fread(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StructContainingPtrReadFromBinFile]

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

testLintSizeofStoredIn32Bits :: Spec
testLintSizeofStoredIn32Bits =
  describe "lintSizeofStoredIn32Bits" $ do
    shouldLintTo "rewrites int variable storing sizeof result to size_t"
      "void f() { int n; n = sizeof(int); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      "size_t"
    shouldLintExactly "exact output for sizeof stored in int rewrite"
      "void f() { int n; n = sizeof(int); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      "void f() { size_t n; n = sizeof(int); }"

testLintHardCodedStructSizes :: Spec
testLintHardCodedStructSizes =
  describe "lintHardCodedStructSizes" $ do
    shouldLeaveUnresolved "leaves HardCodedStructSizes unresolved"
      "void f() { void *p = malloc(16); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [HardCodedStructSizes]

    -- Edge cases
    shouldLeaveUnresolved "leaves StructContainingPtrWrittenToBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; FILE *fp; fwrite(&s, sizeof(s), 1, fp); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      [StructContainingPtrWrittenToBinFile]

    shouldLintTo "rewrites int variable storing sizeof result to size_t (edge: unsigned int)"
      "void f() { unsigned int n; n = sizeof(long); }"
      analyzeAlignmentIssues
      lintAlignmentIssues
      "size_t"
