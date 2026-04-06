module Transformation.BitManipulation where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Transformation.Helpers

transformBitManipulationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformBitManipulationIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      PackingPtrsWithFlagsInInt   -> transformPackingPtrsWithFlagsInInt   a issue
      BitShiftsOnPtr              -> transformBitShiftsOnPtr              a issue
      ExtractingPtrBitsIn32BitVar -> transformExtractingPtrBitsIn32BitVar a issue
      _                           -> (a, Just issue)

-- Cannot be done automatically: packing a 64-bit pointer into a 32-bit int
-- is fundamentally lossy — the upper 32 bits are discarded. The programmer
-- must redesign the scheme (e.g., use intptr_t, a struct, or a tagged pointer
-- exploiting guaranteed alignment), and the correct approach depends on intent.
transformPackingPtrsWithFlagsInInt :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPackingPtrsWithFlagsInInt = untransformable

-- Cannot be done automatically: shifting a pointer is undefined behavior in C.
-- The programmer's intent is unknown — possible purposes include hashing,
-- address compression, or flag extraction — and each requires a different fix.
-- There is no safe mechanical transformation of undefined behavior.
transformBitShiftsOnPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformBitShiftsOnPtr = untransformable

-- Cannot be done automatically: (int)(ptr >> N) stores part of a 64-bit pointer
-- in a 32-bit variable, losing bits. The correct receiving type (intptr_t,
-- uint32_t, uint64_t) and whether the algorithm remains correct on 64-bit both
-- depend on what the programmer intended to extract from the pointer.
transformExtractingPtrBitsIn32BitVar :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformExtractingPtrBitsIn32BitVar = untransformable
