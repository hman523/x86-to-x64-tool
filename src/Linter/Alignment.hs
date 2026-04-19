{-# LANGUAGE LambdaCase #-}
module Linter.Alignment
  ( lintAlignmentIssues
  ) where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintAlignmentIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintAlignmentIssues = dispatchLinter $ \case
    StructContainingPtrWrittenToBinFile -> Just lintStructContainingPtrWrittenToBinFile
    StructContainingPtrReadFromBinFile  -> Just lintStructContainingPtrReadFromBinFile
    StructsWithMixedPtrNonPtrMembers   -> Just lintStructsWithMixedPtrNonPtrMembers
    UnionsContainingPtrAndInts         -> Just lintUnionsContainingPtrAndInts
    PackedStructsWithPtrs              -> Just lintPackedStructsWithPtrs
    SizeofStoredIn32Bits               -> Just lintSizeofStoredIn32Bits
    HardCodedStructSizes               -> Just lintHardCodedStructSizes
    _                                  -> Nothing

-- Cannot be done automatically: the struct's pointer fields make the binary
-- layout non-portable. Fixing this requires redesigning the serialization
-- protocol (e.g., serializing pointed-to data, using offsets instead of
-- pointers, or switching to a format library). The correct approach depends
-- entirely on the protocol's requirements and cannot be determined from the
-- AST alone.
lintStructContainingPtrWrittenToBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintStructContainingPtrWrittenToBinFile = unlintable

-- Cannot be done automatically: data written by a 32-bit process contains
-- 32-bit pointer values in the stream. Correct deserialization requires
-- programmer-defined logic to reconstruct the 64-bit pointers; no
-- mechanical rewrite of the read site can recover the original semantics.
lintStructContainingPtrReadFromBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintStructContainingPtrReadFromBinFile = unlintable

-- Cannot be done automatically: mixed pointer/integer structs will have
-- different padding on 64-bit. Valid fixes include reordering members,
-- adding explicit padding, or splitting the struct, but the right choice
-- depends on layout requirements (e.g., ABI compatibility, wire format)
-- that are not visible in the AST.
lintStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintStructsWithMixedPtrNonPtrMembers = unlintable

-- Cannot be done automatically: the union's integer member is likely too
-- narrow to hold a 64-bit pointer. Whether to widen it to intptr_t (for
-- type-punning) or restructure the union entirely (for a discriminated
-- union) depends on the programmer's intent, which is not recoverable from
-- the AST.
lintUnionsContainingPtrAndInts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUnionsContainingPtrAndInts = unlintable

-- Cannot be done automatically: removing __attribute__((packed)) restores
-- pointer alignment but changes the binary layout; using unaligned accessors
-- preserves layout but requires different generated code. Neither option can
-- be chosen without knowing whether the layout is part of an ABI contract.
lintPackedStructsWithPtrs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintPackedStructsWithPtrs = unlintable

lintSizeofStoredIn32Bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintSizeofStoredIn32Bits ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)

-- Cannot be done automatically: the tool cannot reverse-engineer which struct
-- type the literal corresponds to, so it cannot replace the literal with
-- malloc(sizeof(struct Foo)). The programmer must identify the intended type
-- and substitute the appropriate sizeof expression.
lintHardCodedStructSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintHardCodedStructSizes = unlintable
