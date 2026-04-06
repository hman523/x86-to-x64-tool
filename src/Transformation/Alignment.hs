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

transformStructContainingPtrWrittenToBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructContainingPtrWrittenToBinFile ast issue = (ast, Just issue)

transformStrucContainingPtrReadFromBinFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStrucContainingPtrReadFromBinFile ast issue = (ast, Just issue)

transformStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformStructsWithMixedPtrNonPtrMembers ast issue = (ast, Just issue)

transformUnionsContainingPtrAndInts :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUnionsContainingPtrAndInts ast issue = (ast, Just issue)

transformPackedStructsWithPtrs :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPackedStructsWithPtrs ast issue = (ast, Just issue)

transformSizeofStoredin32bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSizeofStoredin32bits ast issue = case issueDeclPos issue of
    Just ni -> (retypeDecl ni (typedefSpec "size_t") ast, Nothing)
    Nothing -> (ast, Just issue)

transformHardCodedStructSizes :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformHardCodedStructSizes ast issue = (ast, Just issue)
