module Transformation.Alignment where

import Language.C.Syntax.AST
import Analysis.UtilTypes

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

transformStructContainingPtrWrittenToBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructContainingPtrWrittenToBinFile _ issue = undefined

transformStrucContainingPtrReadFromBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStrucContainingPtrReadFromBinFile _ issue = undefined

transformStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructsWithMixedPtrNonPtrMembers _ issue = undefined

transformUnionsContainingPtrAndInts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUnionsContainingPtrAndInts _ issue = undefined

transformPackedStructsWithPtrs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPackedStructsWithPtrs _ issue = undefined

transformSizeofStoredin32bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeofStoredin32bits _ issue = undefined

transformHardCodedStructSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHardCodedStructSizes _ issue = undefined
