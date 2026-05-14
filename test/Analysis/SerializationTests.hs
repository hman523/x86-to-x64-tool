module Analysis.SerializationTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Serialization
import Analysis.IssueTypes

serializationSpec :: Spec
serializationSpec = describe "Serialization Analysis" $ do

    describe "checkWritingPtrDirectToFile" $ do
        shouldFlagError
            "flags fwrite of a pointer-to-pointer buffer"
            "void foo() { int **pp; FILE *f; fwrite(pp, sizeof(*pp), 1, f); }"
            checkWritingPtrDirectToFile

        shouldNotFlagError
            "does not flag fwrite of pointer to int"
            "void foo() { int *p; FILE *f; fwrite(p, sizeof(*p), 10, f); }"
            checkWritingPtrDirectToFile

    describe "checkWritingPtrContainingStructsToFiles" $ do
        shouldFlagError
            "flags fwrite of struct-with-pointer-member address"
            "struct Node { int *ptr; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); }"
            checkWritingPtrContainingStructsToFiles

        shouldNotFlagError
            "does not flag fwrite of struct without pointer members"
            "struct Plain { int x; int y; }; void foo() { struct Plain p; FILE *f; fwrite(&p, sizeof(p), 1, f); }"
            checkWritingPtrContainingStructsToFiles

    describe "checkPtrInSharedMemory" $ do
        shouldFlagError
            "flags shm_open calls"
            "void foo() { int fd = shm_open(\"/test\", 0, 0); }"
            checkPtrInSharedMemory

        shouldFlagError
            "flags shmget calls"
            "void foo() { int id = shmget(0, 1024, 0); }"
            checkPtrInSharedMemory

        shouldNotFlagError
            "does not flag regular open calls"
            "void foo() { int fd = open(\"/test\", 0); }"
            checkPtrInSharedMemory

    describe "checkPtrInMemoryMappedFiles" $ do
        shouldFlagError
            "flags mmap assigned to a pointer-to-pointer variable"
            "void foo() { int **region; region = mmap(0, 4096, 3, 1, -1, 0); }"
            checkPtrInMemoryMappedFiles

        shouldNotFlagError
            "does not flag mmap assigned to a plain pointer"
            "void foo() { int *region; region = mmap(0, 4096, 3, 1, -1, 0); }"
            checkPtrInMemoryMappedFiles

    describe "checkSendingPtrsOverNetwork" $ do  
        shouldFlagError
            "flags send of pointer-to-pointer buffer"
            "void foo() { int **pp; int sock; send(sock, pp, sizeof(*pp), 0); }"
            checkSendingPtrsOverNetwork

        shouldNotFlagError
            "does not flag send of plain buffer"
            "void foo() { char *buf; int sock; send(sock, buf, 100, 0); }"
            checkSendingPtrsOverNetwork

    describe "multiple issues" $ do
        shouldFlagAllTags
            "all four serialization checks fire in one function"
            "struct Rec { int *ptr; int val; }; void foo() { int **pp; FILE *f; fwrite(pp, sizeof(*pp), 1, f); struct Rec r; fwrite(&r, sizeof(r), 1, f); int sock; send(sock, pp, sizeof(*pp), 0); int fd = shm_open(\"/test\", 0, 0); }"
            analyzeSerializationIssues
            [WritingPtrDirectToFile, WritingPtrContainingStructsToFiles, SendingPtrsOverNetwork, PtrInSharedMemory]

        shouldFlagNIssues
            "shm_open and shmget each produce one PtrInSharedMemory issue"
            "void foo() { int fd = shm_open(\"/test\", 0, 0); int id = shmget(0, 1024, 0); }"
            checkPtrInSharedMemory
            2

        shouldFlagNIssues
            "two fwrite calls of pointer-to-pointer buffers produce exactly two issues"
            "void foo() { int **pp; int **qq; FILE *f; fwrite(pp, sizeof(*pp), 1, f); fwrite(qq, sizeof(*qq), 1, f); }"
            checkWritingPtrDirectToFile
            2

    describe "serialization edge cases" $ do

        -- sendto is also in networkSendFns; verify it's covered
        shouldFlagError
            "flags sendto of pointer-to-pointer buffer"
            "void foo() { int **pp; int sock; sendto(sock, pp, sizeof(*pp), 0, 0, 0); }"
            checkSendingPtrsOverNetwork

        -- write() buffer is the 2nd arg; the checker correctly uses args !! 1
        shouldFlagError
            "flags write() of pointer-to-pointer buffer (buf is 2nd arg)"
            "void foo() { int **pp; int fd; write(fd, pp, sizeof(*pp)); }"
            checkWritingPtrDirectToFile

        -- send() with a plain char* buffer (non-pointer data): must NOT flag
        shouldNotFlagError
            "does not flag send() of a plain char* buffer (no nested pointer)"
            "void foo() { char *buf; int sock; int len; send(sock, buf, len, 0); }"
            checkSendingPtrsOverNetwork

        -- fwrite of an address-taken int (scalar, no pointer members): must NOT flag
        shouldNotFlagError
            "does not flag fwrite of an int variable (plain scalar)"
            "void foo() { int x; FILE *f; fwrite(&x, sizeof(x), 1, f); }"
            checkWritingPtrDirectToFile
