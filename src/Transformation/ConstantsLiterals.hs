module Transformation.ConstantsLiterals where

import Language.C.Syntax.AST
import Analysis.IssueTypes

transformConstantsLiteralsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformConstantsLiteralsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      MagicValuesUsed            -> transformMagicValuesUsed            a issue
      BitMaskingAssuming32bitPts -> transformBitMaskingAssuming32bitPts a issue
      HardCodedAddressValues     -> transformHardCodedAddressValues     a issue
      ConstantsUsedForSizeCalcs  -> transformConstantsUsedForSizeCalcs  a issue
      _                          -> (a, Just issue)

transformMagicValuesUsed :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformMagicValuesUsed ast issue = (ast, Just issue)

transformBitMaskingAssuming32bitPts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformBitMaskingAssuming32bitPts ast issue = (ast, Just issue)

transformHardCodedAddressValues :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHardCodedAddressValues ast issue = (ast, Just issue)

transformConstantsUsedForSizeCalcs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformConstantsUsedForSizeCalcs ast issue = (ast, Just issue)
