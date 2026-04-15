module Linter.ConstantsLiterals where

import Language.C.Syntax.AST
import Analysis.IssueTypes
import Linter.Helpers

lintConstantsLiteralsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintConstantsLiteralsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      MagicValuesUsed            -> lintMagicValuesUsed            a issue
      BitMaskingAssuming32bitPts -> lintBitMaskingAssuming32bitPts a issue
      HardCodedAddressValues     -> lintHardCodedAddressValues     a issue
      ConstantsUsedForSizeCalcs  -> lintConstantsUsedForSizeCalcs  a issue
      _                          -> (a, Just issue)

-- Cannot be done automatically: malloc(4) or malloc(8) uses a literal that
-- likely encodes an assumed pointer size. The correct replacement is
-- malloc(sizeof(T)) for some T, but the tool cannot determine which type
-- the programmer intended to allocate.
lintMagicValuesUsed :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintMagicValuesUsed = unlintable

-- Cannot be done automatically: ptr & 0xFFFFFFFF masks to 32 bits, assuming
-- a 32-bit pointer. On 64-bit the programmer may want only the lower 32 bits,
-- the full 64-bit value, or a different mask altogether. Both the mask literal
-- and the receiving variable's type may need to change in a way that depends
-- on intent.
lintBitMaskingAssuming32bitPts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintBitMaskingAssuming32bitPts = unlintable

-- Cannot be done automatically: (T*)0xDEADBEEF is a hardware or MMIO address
-- specific to a 32-bit memory map. The correct 64-bit address is not derivable
-- from the source — it depends on the platform's 64-bit memory map, which is
-- external knowledge the tool does not have.
lintHardCodedAddressValues :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintHardCodedAddressValues = unlintable

-- Cannot be done automatically: a literal integer assigned to a size_t variable
-- (e.g., size_t s = 4) may encode a pointer size, a struct size, or an
-- unrelated count. The tool cannot determine which interpretation is correct,
-- nor what the right 64-bit value would be.
lintConstantsUsedForSizeCalcs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintConstantsUsedForSizeCalcs = unlintable
