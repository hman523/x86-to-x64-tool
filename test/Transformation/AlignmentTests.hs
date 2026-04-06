module Transformation.AlignmentTests where

import Test.Hspec
import Transformation.TransformationTestsUtils
import Transformation.Alignment
import Analysis.Alignment (analyzeAlignmentIssues)
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
    shouldLeaveUnresolved "leaves StructContainingPtrWrittenToBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fwrite(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [StructContainingPtrWrittenToBinFile]

testTransformStrucContainingPtrReadFromBinFile :: Spec
testTransformStrucContainingPtrReadFromBinFile =
  describe "transformStrucContainingPtrReadFromBinFile" $ do
    shouldLeaveUnresolved "leaves StrucContainingPtrReadFromBinFile unresolved"
      "struct S { int *p; void *q; }; void f() { struct S s; fread(&s, sizeof(s), 1, 0); }"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [StrucContainingPtrReadFromBinFile]

testTransformStructsWithMixedPtrNonPtrMembers :: Spec
testTransformStructsWithMixedPtrNonPtrMembers =
  describe "transformStructsWithMixedPtrNonPtrMembers" $ do
    shouldLeaveUnresolved "leaves StructsWithMixedPtrNonPtrMembers unresolved"
      "struct S { int *p; int x; };"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [StructsWithMixedPtrNonPtrMembers]

testTransformUnionsContainingPtrAndInts :: Spec
testTransformUnionsContainingPtrAndInts =
  describe "transformUnionsContainingPtrAndInts" $ do
    shouldLeaveUnresolved "leaves UnionsContainingPtrAndInts unresolved"
      "union U { int *p; int x; };"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [UnionsContainingPtrAndInts]

testTransformPackedStructsWithPtrs :: Spec
testTransformPackedStructsWithPtrs =
  describe "transformPackedStructsWithPtrs" $ do
    shouldLeaveUnresolved "leaves PackedStructsWithPtrs unresolved"
      "struct __attribute__((packed)) S { int *p; void *q; };"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [PackedStructsWithPtrs]

testTransformSizeofStoredin32bits :: Spec
testTransformSizeofStoredin32bits =
  describe "transformSizeofStoredin32bits" $ do
    shouldTransformTo "rewrites int variable storing sizeof result to size_t"
      "void f() { int n; n = sizeof(int); }"
      analyzeAlignmentIssues
      transformAlignmentIssues
      "size_t"

testTransformHardCodedStructSizes :: Spec
testTransformHardCodedStructSizes =
  describe "transformHardCodedStructSizes" $ do
    shouldLeaveUnresolved "leaves HardCodedStructSizes unresolved"
      "void f() { void *p = malloc(16); }"
      analyzeAlignmentIssues
      transformAlignmentIssues
      [HardCodedStructSizes]
