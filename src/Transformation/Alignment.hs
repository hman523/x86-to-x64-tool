module Transformation.Alignment where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

transformAlignmentIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformAlignmentIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      StructContainingPtrWrittenToBinFile -> transformStructContainingPtrWrittenToBinFile a issue
      StrucContainingPtrReadFromBinFile   -> transformStrucContainingPtrReadFromBinFile   a issue
      StructsWithMixedPtrNonPtrMembers    -> transformStructsWithMixedPtrNonPtrMembers    a issue
      UnionsContainingPtrAndInts          -> transformUnionsContainingPtrAndInts          a issue
      PackedStructsWithPtrs               -> transformPackedStructsWithPtrs               a issue
      SizeofStoredin32bits                -> transformSizeofStoredin32bits                a issue
      HardCodedStructSizes                -> transformHardCodedStructSizes                a issue
      _                                   -> (a, Just issue)

-- Cannot be done automatically: the struct's pointer fields make the binary
-- layout non-portable. Fixing this requires redesigning the serialization
-- protocol (e.g., serializing pointed-to data, using offsets instead of
-- pointers, or switching to a format library). The correct approach depends
-- entirely on the protocol's requirements and cannot be determined from the
-- AST alone.
transformStructContainingPtrWrittenToBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructContainingPtrWrittenToBinFile = untransformable

-- Cannot be done automatically: data written by a 32-bit process contains
-- 32-bit pointer values in the stream. Correct deserialization requires
-- programmer-defined logic to reconstruct the 64-bit pointers; no
-- mechanical rewrite of the read site can recover the original semantics.
transformStrucContainingPtrReadFromBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStrucContainingPtrReadFromBinFile = untransformable

-- Cannot be done automatically: mixed pointer/integer structs will have
-- different padding on 64-bit. Valid fixes include reordering members,
-- adding explicit padding, or splitting the struct, but the right choice
-- depends on layout requirements (e.g., ABI compatibility, wire format)
-- that are not visible in the AST.
transformStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructsWithMixedPtrNonPtrMembers = untransformable

-- Cannot be done automatically: the union's integer member is likely too
-- narrow to hold a 64-bit pointer. Whether to widen it to intptr_t (for
-- type-punning) or restructure the union entirely (for a discriminated
-- union) depends on the programmer's intent, which is not recoverable from
-- the AST.
transformUnionsContainingPtrAndInts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUnionsContainingPtrAndInts = untransformable

-- Cannot be done automatically: removing __attribute__((packed)) restores
-- pointer alignment but changes the binary layout; using unaligned accessors
-- preserves layout but requires different generated code. Neither option can
-- be chosen without knowing whether the layout is part of an ABI contract.
transformPackedStructsWithPtrs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPackedStructsWithPtrs = untransformable

transformSizeofStoredin32bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeofStoredin32bits ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)

-- Cannot be done automatically: the tool cannot reverse-engineer which struct
-- type the literal corresponds to, so it cannot replace the literal with
-- malloc(sizeof(struct Foo)). The programmer must identify the intended type
-- and substitute the appropriate sizeof expression.
transformHardCodedStructSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHardCodedStructSizes = untransformable
