module Analysis.Serialization where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeSerializationIssues :: CTranslUnit -> [Issue]
analyzeSerializationIssues ast =
    checkWritingPtrDirectToFile ast
    ++ checkWritingPtrContrainingStructsToFiles ast
    ++ checkSendingPtrsOverNetwork ast
    ++ checkPtrInMemoryMappedFiles ast
    ++ checkPtrInSharedMemory ast

-- ---------------------------------------------------------------------------
-- File / network I/O checks
-- ---------------------------------------------------------------------------

-- | Flag fwrite/write calls whose buffer argument is a pointer-to-pointer
--   (the pointer value itself is being serialised).
checkWritingPtrDirectToFile :: CTranslUnit -> [Issue]
checkWritingPtrDirectToFile ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkWrite tenv) Map.empty) decls
  where
    checkWrite tenv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ["fwrite", "write"], length args >= 1 =
            let bufType = resolveTypedef tenv (typeOfExpr env (head args))
            in [ createIssue info Critical WritingPtrDirectToFile
               | isPtrToPtr bufType ]
    checkWrite _ _ _ = []

    isPtrToPtr (TPointer (TPointer _)) = True
    isPtrToPtr _                        = False

-- | Flag fwrite/write where the buffer is an address of a struct with pointer members.
checkWritingPtrContrainingStructsToFiles :: CTranslUnit -> [Issue]
checkWritingPtrContrainingStructsToFiles ast@(CTranslUnit decls _) =
    let senv = buildStructEnv ast
    in concatMap (analyzeDecl (checkWrite senv) Map.empty) decls
  where
    checkWrite senv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ["fwrite", "write"], length args >= 1 =
            let bufType = typeOfExpr env (head args)
            in [ createIssue info Critical WritingPtrContrainingStructsToFiles
               | ptrToStructWithPtrs senv bufType ]
    checkWrite _ _ _ = []

-- | Flag send/sendto where the buffer is a pointer or a pointer-containing struct.
checkSendingPtrsOverNetwork :: CTranslUnit -> [Issue]
checkSendingPtrsOverNetwork ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
        senv = buildStructEnv ast
    in concatMap (analyzeDecl (checkSend tenv senv) Map.empty) decls
  where
    checkSend tenv senv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ["send", "sendto"], length args >= 2 =
            let bufType = typeOfExpr env (args !! 1)
                bufTypeR = resolveTypedef tenv bufType
            in [ createIssue info Critical SendingPtrsOverNetwork
               | isPtrToPtr bufTypeR || ptrToStructWithPtrs senv bufType ]
    checkSend _ _ _ _ = []

    isPtrToPtr (TPointer (TPointer _)) = True
    isPtrToPtr _                        = False

-- ---------------------------------------------------------------------------
-- IPC / shared-memory checks
-- ---------------------------------------------------------------------------

-- | Flag mmap assignments to pointer-to-pointer variables (pointer stored in map).
checkPtrInMemoryMappedFiles :: CTranslUnit -> [Issue]
checkPtrInMemoryMappedFiles ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkMmap tenv) Map.empty) decls
  where
    checkMmap tenv env (CAssign CAssignOp lhs
            (CCall (CVar (Ident "mmap" _ _) _) _ _) info) =
        let lhsType = resolveTypedef tenv (typeOfExpr env lhs)
        in [ createIssue info Warning PtrInMemoryMappedFiles
           | isPtrToPtr lhsType ]
    checkMmap _ _ _ = []

    isPtrToPtr (TPointer (TPointer _)) = True
    isPtrToPtr _                        = False

-- | Flag calls to shared-memory functions (the region may receive pointers).
checkPtrInSharedMemory :: CTranslUnit -> [Issue]
checkPtrInSharedMemory ast@(CTranslUnit decls _) =
    concatMap (analyzeDecl checkShmCall Map.empty) decls
  where
    checkShmCall _env (CCall (CVar (Ident fname _ _) _) _ info)
        | fname `elem` ["shm_open", "shmget", "shmat"] =
            [createIssue info Warning PtrInSharedMemory]
    checkShmCall _ _ = []

-- ---------------------------------------------------------------------------
-- Shared helper
-- ---------------------------------------------------------------------------

-- | True when @t@ is TPointer (TStruct name) and that struct has pointer members.
ptrToStructWithPtrs :: StructEnv -> CType -> Bool
ptrToStructWithPtrs senv (TPointer (TStruct name)) = structHasPointer senv name
ptrToStructWithPtrs _ _                             = False