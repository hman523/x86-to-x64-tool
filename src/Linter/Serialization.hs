{-# LANGUAGE LambdaCase #-}
module Linter.Serialization
  ( lintSerializationIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintSerializationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintSerializationIssues = dispatchLinter $ \case
    WritingPtrDirectToFile              -> Just lintWritingPtrDirectToFile
    WritingPtrContainingStructsToFiles -> Just lintWritingPtrContainingStructsToFiles
    SendingPtrsOverNetwork              -> Just lintSendingPtrsOverNetwork
    PtrInMemoryMappedFiles              -> Just lintPtrInMemoryMappedFiles
    PtrInSharedMemory                   -> Just lintPtrInSharedMemory
    _                                   -> Nothing

-- Cannot be done automatically: a raw pointer value is process-local and
-- meaningless once deserialized. Fixing this requires switching to a portable
-- serialization scheme (e.g., writing indices, offsets, or the pointed-to
-- data itself), all of which require redesigning the data format — a decision
-- the tool cannot make on the programmer's behalf.
lintWritingPtrDirectToFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintWritingPtrDirectToFile = unlintable

-- Cannot be done automatically: a struct containing pointer fields is written
-- raw via fwrite. The pointer fields in the serialized bytes are not portable
-- across process boundaries or address-space layouts. The programmer must
-- define a format that replaces pointers with relocatable representations
-- (offsets, indices, etc.).
lintWritingPtrContainingStructsToFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintWritingPtrContainingStructsToFiles = unlintable

-- Cannot be done automatically: pointer values are virtual addresses in the
-- sending process's address space and are meaningless to the receiver. The
-- serialization protocol must be redesigned to use portable identifiers
-- (handles, indices, or serialized data), which requires understanding the
-- protocol's semantics.
lintSendingPtrsOverNetwork :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintSendingPtrsOverNetwork = unlintable

-- Cannot be done automatically: a pointer stored in a memory-mapped file will
-- have a different virtual address in every process that maps the file. The
-- programmer must switch to an offset-based or index-based scheme, and the
-- right representation depends on the memory map's layout — external knowledge
-- not present in the AST.
lintPtrInMemoryMappedFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPtrInMemoryMappedFiles = unlintable

-- Cannot be done automatically: pointers stored in shared memory have different
-- values in each process due to ASLR and distinct virtual address spaces. The
-- programmer must redesign the shared data structure to use offsets relative to
-- the shared region's base address, which depends on the structure's layout and
-- usage pattern.
lintPtrInSharedMemory :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPtrInSharedMemory = unlintable
