module Analysis.IntegrationTests where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import Parser.Parser (parseSource)
import Analysis.Analysis (analysis)
import Analysis.AnalysisTestUtils
import Analysis.IssueTypes
import Data.List (nub, (\\))

integrationSpec :: Spec
integrationSpec = do
  everythingAtOnce <- runIO $ BS.readFile "test/c_progs/everything_at_once.c"

  describe "Integration Tests" $ do

    describe "legacy memory manager" $ do
        shouldFlagAllTags
            "pointer diff to int, multiply overflow malloc, add overflow malloc, sizeof to int, pointer cast all fire"
            "void foo() { int *a; int *b; int diff; diff = a - b; int n; int m; char *p = malloc(n * m); char *q = malloc(n + m); int sz; sz = sizeof(void*); int addr = (int)a; }"
            analysis
            [PtrDiffStoredAs32bit, AllocationSizeCalcsMayOverflow, MallocWithoutOverflowChecking, UsingIntToStoreAllocationSizes, CastPointerToInt]

        shouldFlagAtLeastNIssues
            "legacy memory manager snippet triggers at least five issues"
            "void foo() { int *a; int *b; int diff; diff = a - b; int n; int m; char *p = malloc(n * m); char *q = malloc(n + m); int sz; sz = sizeof(void*); int addr = (int)a; }"
            analysis
            5

    describe "array traversal utility" $ do
        shouldFlagAllTags
            "int loop counter over ptr range, int array index, ptr+int, and ptr-vs-literal all flagged"
            "void foo() { int *start; int *end; for (int i = 0; i < (end - start); i++) { } int arr[10]; int idx; int x = arr[idx]; int n; int *q = start + n; if (start < 4096) { } }"
            analysis
            [LoopCounterAsIntWhenIteratingOverPtrArrays, ArrayIndexingIntInArrayOver2tothe31size, PointerAddOverflow, PtrComparisonWithIntConsts]

        shouldFlagAtLeastNIssues
            "array traversal utility triggers at least four issues"
            "void foo() { int *start; int *end; for (int i = 0; i < (end - start); i++) { } int arr[10]; int idx; int x = arr[idx]; int n; int *q = start + n; if (start < 4096) { } }"
            analysis
            4

    describe "type-unsafe serializer" $ do
        shouldFlagAllTags
            "mixed-member struct, fwrite of ptr-to-ptr, fwrite of struct with ptr, and send of ptr-to-ptr all flagged"
            "struct Record { int *data; int len; }; void foo() { int **pp; FILE *f; fwrite(pp, sizeof(*pp), 1, f); struct Record r; fwrite(&r, sizeof(r), 1, f); int sock; send(sock, pp, sizeof(*pp), 0); }"
            analysis
            [StructsWithMixedPtrNonPtrMembers, WritingPtrDirectToFile, WritingPtrContainingStructsToFiles, SendingPtrsOverNetwork]

        shouldFlagAtLeastNIssues
            "type-unsafe serializer triggers at least four issues"
            "struct Record { int *data; int len; }; void foo() { int **pp; FILE *f; fwrite(pp, sizeof(*pp), 1, f); struct Record r; fwrite(&r, sizeof(r), 1, f); int sock; send(sock, pp, sizeof(*pp), 0); }"
            analysis
            4

    describe "bit twiddling engine" $ do
        shouldFlagAllTags
            "pointer packing, bit shift on pointer, extracting pointer bits, and int cast of pointer all flagged"
            "void foo() { int *p; int flags; int packed = (int)(p | flags); int n; long shifted = p << n; int bits = (int)(p >> 32); int addr = (int)p; }"
            analysis
            [PackingPtrsWithFlagsInInt, BitShiftsOnPtr, ExtractingPtrBitsIn32BitVar, CastPointerToInt]

        shouldFlagAtLeastNIssues
            "bit twiddling engine triggers at least four issues"
            "void foo() { int *p; int flags; int packed = (int)(p | flags); int n; long shifted = p << n; int bits = (int)(p >> 32); int addr = (int)p; }"
            analysis
            4

    describe "format string logger" $ do
        shouldFlagAllTags
            "percent-d, percent-x, percent-lu, and percent-ld format mismatches all flagged"
            "void foo() { int *p; long n; printf(\"%d\", p); printf(\"%x\", p); printf(\"%lu\", p); printf(\"%ld\", n); }"
            analysis
            [DUsedWithPtr, XUsedWithPtr, LuUsedForPtrSizedVals, LdUsedWithLongAssuming64bits]

        shouldFlagAtLeastNIssues
            "format string logger triggers at least four issues"
            "void foo() { int *p; long n; printf(\"%d\", p); printf(\"%x\", p); printf(\"%lu\", p); printf(\"%ld\", n); }"
            analysis
            4

    describe "platform-specific pass" $ do
        shouldFlagAllTags
            "x86 asm, sizeof==4 assumption, SSE intrinsic, and long-to-pointer cast all flagged"
            "void foo() { __asm__(\"movl %eax, %ebx\"); if (sizeof(int) == 4) { } _mm_set1_ps(0); long base = 0x1000; int *p = (int*)base; }"
            analysis
            [AsmBlocks, InlineAsmWithx86Instructions, AssumptionsAboutRegSizes, X86SpecificCompilerIntrinsics, CastLongToPointer]

        shouldFlagAtLeastNIssues
            "platform-specific pass triggers at least five issues"
            "void foo() { __asm__(\"movl %eax, %ebx\"); if (sizeof(int) == 4) { } _mm_set1_ps(0); long base = 0x1000; int *p = (int*)base; }"
            analysis
            5

    describe "file I/O utility" $ do
        shouldFlagAllTags
            "pointer diff to int, int file offset, printf of pointer with percent-d, and sizeof to int all flagged"
            "void foo() { int *start; int *end; int diff; diff = end - start; int offset; fseek(0, offset, 0); int *p; printf(\"%d\", p); int sz; sz = sizeof(void*); }"
            analysis
            [PtrDiffStoredAs32bit, UsingIntForFileOffsets, DUsedWithPtr, UsingIntToStoreAllocationSizes]

        shouldFlagAtLeastNIssues
            "file I/O utility triggers at least four issues"
            "void foo() { int *start; int *end; int diff; diff = end - start; int offset; fseek(0, offset, 0); int *p; printf(\"%d\", p); int sz; sz = sizeof(void*); }"
            analysis
            4

    describe "IPC shared memory" $ do
        shouldFlagAllTags
            "mixed-member struct, ptr-and-int union, shm_open, and send of struct with pointer all flagged"
            "struct Msg { int seq; int *payload; }; union Handle { int fd; int *ptr; }; void foo() { int fd = shm_open(\"/test\", 0, 0); struct Msg m; int sock; send(sock, &m, sizeof(m), 0); }"
            analysis
            [StructsWithMixedPtrNonPtrMembers, UnionsContainingPtrAndInts, PtrInSharedMemory, SendingPtrsOverNetwork]

        shouldFlagAtLeastNIssues
            "IPC shared memory snippet triggers at least four issues"
            "struct Msg { int seq; int *payload; }; union Handle { int fd; int *ptr; }; void foo() { int fd = shm_open(\"/test\", 0, 0); struct Msg m; int sock; send(sock, &m, sizeof(m), 0); }"
            analysis
            4

    describe "hash table lookup" $ do
        shouldFlagAllTags
            "int cast of pointer, int loop over ptr range, ptr vs literal, printf with percent-d, and magic malloc all flagged"
            "void foo() { int *start; int *end; int addr = (int)start; for (int i = 0; i < (end - start); i++) { } int *p; if (p < 4096) { } printf(\"%d\", p); char *buf = malloc(4); }"
            analysis
            [CastPointerToInt, LoopCounterAsIntWhenIteratingOverPtrArrays, PtrComparisonWithIntConsts, DUsedWithPtr, MagicValuesUsed]

        shouldFlagAtLeastNIssues
            "hash table lookup triggers at least five issues"
            "void foo() { int *start; int *end; int addr = (int)start; for (int i = 0; i < (end - start); i++) { } int *p; if (p < 4096) { } printf(\"%d\", p); char *buf = malloc(4); }"
            analysis
            5

    describe "struct serialization pipeline" $ do
        shouldFlagAllTags
            "mixed-member struct, ptr diff to int, multiply overflow malloc, fwrite of struct with ptr, send of struct with ptr, sizeof to int, and ptr vs literal all flagged"
            "struct Node { int *next; int val; }; void foo() { int *start; int *end; int diff; diff = end - start; int n; int m; char *buf = malloc(n * m); struct Node nd; FILE *f; fwrite(&nd, sizeof(nd), 1, f); int sock; send(sock, &nd, sizeof(nd), 0); int sz; sz = sizeof(int *); int *p; if (p < 100) { } }"
            analysis
            [StructsWithMixedPtrNonPtrMembers, PtrDiffStoredAs32bit, AllocationSizeCalcsMayOverflow, WritingPtrContainingStructsToFiles, SendingPtrsOverNetwork, SizeofStoredIn32Bits, PtrComparisonWithIntConsts]

        shouldFlagAtLeastNIssues
            "struct serialization pipeline triggers at least seven issues"
            "struct Node { int *next; int val; }; void foo() { int *start; int *end; int diff; diff = end - start; int n; int m; char *buf = malloc(n * m); struct Node nd; FILE *f; fwrite(&nd, sizeof(nd), 1, f); int sock; send(sock, &nd, sizeof(nd), 0); int sz; sz = sizeof(int *); int *p; if (p < 100) { } }"
            analysis
            7

    -- One representative issue from every distinct IssueTag category,
    -- sourced from the canonical test file on disk.
    describe "everything at once" $ do
        let expectedTags =
              [ StructsWithMixedPtrNonPtrMembers
              , UnionsContainingPtrAndInts
              , StructContainingPtrWrittenToBinFile
              , SizeofStoredIn32Bits
              , HardCodedStructSizes
              , PackingPtrsWithFlagsInInt
              , BitShiftsOnPtr
              , ExtractingPtrBitsIn32BitVar
              , LoopCounterAsIntWhenIteratingOverPtrArrays
              , PtrComparisonWithIntConsts
              , UsingIntForFileOffsets
              , MagicValuesUsed
              , HardCodedAddressValues
              , BitMaskingAssuming32bitPts
              , ConstantsUsedForSizeCalcs
              , DUsedWithPtr
              , LdUsedWithLongAssuming64bits
              , FnsReturnPtrAsInt
              , FnsParamDeclaredAsIntTakesPtr
              , AllocationSizeCalcsMayOverflow
              , MallocWithoutOverflowChecking
              , UsingIntToStoreAllocationSizes
              , AsmBlocks
              , InlineAsmWithx86Instructions
              , AssumptionsAboutRegSizes
              , X86SpecificCompilerIntrinsics
              , PtrDiffStoredAs32bit
              , PointerAddOverflow
              , PtrSubUnderflow
              , ArrayIndexingIntInArrayOver2tothe31size
              , WritingPtrDirectToFile
              , WritingPtrContainingStructsToFiles
              , SendingPtrsOverNetwork
              , PtrInSharedMemory
              , CastPointerToInt
              , CastLongToPointer
              , SizeOfIntIsVoid
              , UsingUIntAsMemSize
              ]

        it "one issue from every category fires from a single large problematic translation unit" $ do
            case parseSource everythingAtOnce of
              Left err  -> fail $ show err
              Right ast -> do
                let foundTags = map issueType (analysis ast)
                let missing   = nub expectedTags \\ nub foundTags
                missing `shouldBe` []

        it "everything-at-once file triggers at least 38 distinct issues" $ do
            case parseSource everythingAtOnce of
              Left err  -> fail $ show err
              Right ast -> length (analysis ast) `shouldSatisfy` (>= 38)
