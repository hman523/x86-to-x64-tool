module Transformation.Serialization where

import Language.C.Syntax.AST
import Analysis.UtilTypes

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
transformWritingPtrDirectToFile _ issue = undefined

transformWritingPtrContrainingStructsToFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformWritingPtrContrainingStructsToFiles _ issue = undefined

transformSendingPtrsOverNetwork :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformSendingPtrsOverNetwork _ issue = undefined

transformPtrInMemoryMappedFiles :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrInMemoryMappedFiles _ issue = undefined

transformPtrInSharedMemory :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformPtrInSharedMemory _ issue = undefined
