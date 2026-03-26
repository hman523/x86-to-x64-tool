module Analysis.SerializationTests where

import Test.Hspec
import Analysis.AnalysisTestUtils
import Analysis.Serialization
import Analysis.UtilTypes

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

    describe "checkWritingPtrContrainingStructsToFiles" $ do
        shouldFlagError
            "flags fwrite of struct-with-pointer-member address"
            "struct Node { int *ptr; int val; }; void foo() { struct Node n; FILE *f; fwrite(&n, sizeof(n), 1, f); }"
            checkWritingPtrContrainingStructsToFiles

        shouldNotFlagError
            "does not flag fwrite of struct without pointer members"
            "struct Plain { int x; int y; }; void foo() { struct Plain p; FILE *f; fwrite(&p, sizeof(p), 1, f); }"
            checkWritingPtrContrainingStructsToFiles

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

    describe "checkSendingPtrsOverNetwork" $ do  
        shouldFlagError
            "flags send of pointer-to-pointer buffer"
            "void foo() { int **pp; int sock; send(sock, pp, sizeof(*pp), 0); }"
            checkSendingPtrsOverNetwork

        shouldNotFlagError
            "does not flag send of plain buffer"
            "void foo() { char *buf; int sock; send(sock, buf, 100, 0); }"
            checkSendingPtrsOverNetwork
