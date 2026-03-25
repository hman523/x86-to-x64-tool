module Analysis.Serialization where

import Language.C.Syntax.AST
import Analysis.UtilTypes

analyzeSerializationIssues :: CTranslUnit -> [Issue]
analyzeSerializationIssues ast =
    checkWritingPtrDirectToFile ast
    ++ checkWritingPtrContrainingStructsToFiles ast
    ++ checkSendingPtrsOverNetwork ast
    ++ checkPtrInMemoryMappedFiles ast
    ++ checkPtrInSharedMemory ast

-- writingPtrDirectToFile
checkWritingPtrDirectToFile :: CTranslUnit -> [Issue]
checkWritingPtrDirectToFile ast = []

-- writingPtrContrainingStructsToFiles
checkWritingPtrContrainingStructsToFiles :: CTranslUnit -> [Issue]
checkWritingPtrContrainingStructsToFiles ast = []

-- sendingPtrsOverNetwork
checkSendingPtrsOverNetwork :: CTranslUnit -> [Issue]
checkSendingPtrsOverNetwork ast = []

-- ptrInMemoryMappedFiles
checkPtrInMemoryMappedFiles :: CTranslUnit -> [Issue]
checkPtrInMemoryMappedFiles ast = []

-- ptrInSharedMemory
checkPtrInSharedMemory :: CTranslUnit -> [Issue]
checkPtrInSharedMemory ast = []