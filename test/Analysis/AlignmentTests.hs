module Analysis.AlignmentTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Alignment
import Analysis.IssueTypes

alignmentSpec :: Spec
alignmentSpec = describe "Alignment Analysis" $ do

    describe "checkStructsWithMixedPtrNonPtrMembers" $ do
        shouldFlagError
            "flags struct with both pointer and int members"
            "struct Foo { int *ptr; int val; };"
            checkStructsWithMixedPtrNonPtrMembers

        shouldFlagError
            "flags struct with pointer and unsigned int members"
            "struct Foo { int *ptr; unsigned int val; };"
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

        shouldFlagError
            "flags union with pointer and unsigned int members"
            "union Bar { int *ptr; unsigned int val; };"
            checkUnionsContainingPtrAndInts

        shouldNotFlagError
            "does not flag struct with mixed members (only flags unions)"
            "struct Bar { int *ptr; int val; };"
            checkUnionsContainingPtrAndInts

    describe "checkPackedStructsWithPtrs" $ do
        shouldFlagError
            "flags packed struct containing a pointer member"
            "struct __attribute__((packed)) Pkt { int *ptr; int val; };"
            checkPackedStructsWithPtrs

        shouldNotFlagError
            "does not flag packed struct with no pointers"
            "struct __attribute__((packed)) Pkt { int a; int b; };"
            checkPackedStructsWithPtrs

        shouldNotFlagError
            "does not flag non-packed struct with pointer"
            "struct Foo { int *ptr; int val; };"
            checkPackedStructsWithPtrs

    describe "checkStructContainingPtrReadFromBinFile" $ do
        shouldFlagError
            "flags fread into struct with pointer member"
            "struct Node { int *next; int val; }; void foo() { struct Node n; FILE *f; fread(&n, sizeof(n), 1, f); }"
            checkStructContainingPtrReadFromBinFile

        shouldNotFlagError
            "does not flag fread of plain int buffer"
            "void foo() { int buf; FILE *f; fread(&buf, sizeof(buf), 1, f); }"
            checkStructContainingPtrReadFromBinFile

    describe "checkSizeofStoredIn32bits" $ do
        shouldFlagError
            "flags sizeof result assigned to int variable"
            "void foo() { int sz; sz = sizeof(int *); }"
            checkSizeofStoredIn32bits

        shouldFlagError
            "flags sizeof result assigned to unsigned int variable"
            "void foo() { unsigned int sz; sz = sizeof(int *); }"
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

    describe "edge cases" $ do

        -- fwrite of a struct-with-pointer — parallel to fread (the fwrite describe is missing)
        describe "checkStructContainingPtrWrittenToBinFile" $ do
            shouldFlagError
                "flags fwrite of struct-with-pointer-member address"
                "struct Node { int *next; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); }"
                checkStructContainingPtrWrittenToBinFile

            shouldNotFlagError
                "does not flag fwrite of plain int buffer"
                "void foo() { int buf; FILE *f; fwrite(&buf, sizeof(buf), 1, f); }"
                checkStructContainingPtrWrittenToBinFile

        -- struct with a function pointer member counts as having a pointer
        shouldFlagError
            "flags struct with function pointer member and int member"
            "struct Handler { void (*callback)(int); int state; };"
            checkStructsWithMixedPtrNonPtrMembers

        -- sizeof stored in unsigned int is also a truncation risk
        shouldFlagError
            "flags sizeof(int*) stored in unsigned int variable"
            "void foo() { unsigned int sz; sz = sizeof(int*); }"
            checkSizeofStoredIn32bits

        -- nested struct: the outer struct has a plain int and the inner has a pointer;
        -- the outer struct itself contains a mixed-layout embedded struct
        shouldFlagError
            "flags outer struct when it contains a struct member with a pointer and has its own int member"
            "struct Inner { int *ptr; }; struct Outer { struct Inner inner; int val; };"
            checkStructsWithMixedPtrNonPtrMembers

        -- anonymous union inside a struct with a pointer member
        shouldFlagError
            "flags union containing a pointer member and an int member"
            "union Variant { int *ptr; int raw; };"
            checkUnionsContainingPtrAndInts

        -- struct with only function pointers (no plain ints): should not flag mixed-members
        shouldNotFlagError
            "does not flag struct with only function pointer members"
            "struct Callbacks { void (*on_open)(int); void (*on_close)(int); };"
            checkStructsWithMixedPtrNonPtrMembers

    describe "checkStructContainingPtrWrittenToBinFile" $ do
        shouldFlagError
            "flags fwrite of pointer to struct with pointer member"
            "struct Node { int *next; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); }"
            checkStructContainingPtrWrittenToBinFile

        shouldNotFlagError
            "does not flag fwrite of plain int"
            "void foo() { int x = 42; FILE *f; fwrite(&x, sizeof(x), 1, f); }"
            checkStructContainingPtrWrittenToBinFile

    describe "multiple issues" $ do
        shouldFlagAllTags
            "four alignment checks fire from struct, union, sizeof, and malloc in one snippet"
            "struct Foo { int *p; int x; }; union Bar { int *ptr; int val; }; void foo() { int sz; sz = sizeof(int *); void *m = malloc(32); }"
            analyzeAlignmentIssues
            [StructsWithMixedPtrNonPtrMembers, UnionsContainingPtrAndInts, SizeofStoredIn32Bits, HardCodedStructSizes]

        shouldFlagNIssues
            "two mixed-member struct definitions produce exactly two issues"
            "struct A { int *p; int x; }; struct B { char *s; int n; };"
            checkStructsWithMixedPtrNonPtrMembers
            2

        shouldFlagAtLeastNIssues
            "struct definition, fwrite of struct with pointer, and sizeof-to-int together produce at least three issues"
            "struct Node { int *next; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); int sz; sz = sizeof(int *); }"
            analyzeAlignmentIssues
            3

    describe "nested struct edge cases" $ do

        shouldFlagError
            "outer struct with pointer and int members is flagged even when a nested struct member is also present"
            "struct Inner { int z; }; struct Outer { int *p; int x; struct Inner sub; };"
            checkStructsWithMixedPtrNonPtrMembers

        shouldFlagError
            "inner struct with pointer and int members is flagged independently of the outer struct"
            "struct Inner { int *next; int val; }; struct Outer { struct Inner sub; };"
            checkStructsWithMixedPtrNonPtrMembers

        shouldFlagNIssues
            "two structs both with mixed pointer/int members produce two issues"
            "struct A { int *p; int x; }; struct B { char *s; long n; };"
            checkStructsWithMixedPtrNonPtrMembers
            2

        shouldNotFlagError
            "struct with only pointer members is not flagged even when nested struct present"
            "struct Inner { int y; }; struct Outer { int *p; char *s; };"
            checkStructsWithMixedPtrNonPtrMembers
