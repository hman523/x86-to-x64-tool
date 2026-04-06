module Transformation.Serialization where

import Language.C.Syntax.AST
import Analysis.IssueTypes

transformSerializationIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformSerializationIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      WritingPtrDirectToFile              -> transformWritingPtrDirectToFile              a issue
      WritingPtrContrainingStructsToFiles -> transformWritingPtrContrainingStructsToFiles a issue
      SendingPtrsOverNetwork              -> transformSendingPtrsOverNetwork              a issue
      PtrInMemoryMappedFiles              -> transformPtrInMemoryMappedFiles              a issue
      PtrInSharedMemory                   -> transformPtrInSharedMemory                   a issue
      _                                   -> (a, Just issue)

transformWritingPtrDirectToFile :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformWritingPtrDirectToFile ast issue = (ast, Just issue)

transformWritingPtrContrainingStructsToFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformWritingPtrContrainingStructsToFiles ast issue = (ast, Just issue)

transformSendingPtrsOverNetwork :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSendingPtrsOverNetwork ast issue = (ast, Just issue)

transformPtrInMemoryMappedFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrInMemoryMappedFiles ast issue = (ast, Just issue)

transformPtrInSharedMemory :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrInSharedMemory ast issue = (ast, Just issue)
