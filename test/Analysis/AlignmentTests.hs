module Analysis.AlignmentTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Alignment
import Analysis.UtilTypes

alignmentSpec :: Spec
alignmentSpec = describe "Alignment Analysis" $ do

    describe "checkStructsWithMixedPtrNonPtrMembers" $ do
        shouldFlagError
            "flags struct with both pointer and int members"
            "struct Foo { int *ptr; int val; };"
            checkStructsWithMixedPtrNonPtrMembers

        shouldNotFlagError
            "does not flag struct with only pointer members"
            "struct Foo { int *a; char *b; };"
            checkStructsWithMixedPtrNonPtrMembers

        shouldNotFlagError
            "does not flag struct with only int members"
            "struct Foo { int a; int b; };"
            checkStructsWithMixedPtrNonPtrMembers

    describe "checkUnionsContainingPtrAndInts" $ do
        shouldFlagError
            "flags union with both pointer and int members"
            "union Bar { int *ptr; int val; };"
            checkUnionsContainingPtrAndInts

        shouldNotFlagError
            "does not flag struct with mixed members (only flags unions)"
            "struct Bar { int *ptr; int val; };"
            checkUnionsContainingPtrAndInts

    describe "checkSizeofStoredIn32bits" $ do
        shouldFlagError
            "flags sizeof result assigned to int variable"
            "void foo() { int sz; sz = sizeof(int *); }"
            checkSizeofStoredIn32bits

        shouldNotFlagError
            "does not flag sizeof result assigned to long"
            "void foo() { long sz; sz = sizeof(int *); }"
            checkSizeofStoredIn32bits

    describe "checkHardCodedStructSizes" $ do
        shouldFlagError
            "flags malloc with raw integer literal larger than 8"
            "void foo() { void *p = malloc(16); }"
            checkHardCodedStructSizes

        shouldNotFlagError
            "does not flag malloc with sizeof expression"
            "struct Foo { int x; }; void foo() { void *p = malloc(sizeof(struct Foo)); }"
            checkHardCodedStructSizes

    describe "checkStructContainingPtrWrittenToBinFile" $ do
        shouldFlagError
            "flags fwrite of pointer to struct with pointer member"
            "struct Node { int *next; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); }"
            checkStructContainingPtrWrittenToBinFile

        shouldNotFlagError
            "does not flag fwrite of plain int"
            "void foo() { int x = 42; FILE *f; fwrite(&x, sizeof(x), 1, f); }"
            checkStructContainingPtrWrittenToBinFile
