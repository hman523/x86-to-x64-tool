module Analysis.Alignment
  ( analyzeAlignmentIssues
  , checkStructsWithMixedPtrNonPtrMembers
  , checkUnionsContainingPtrAndInts
  , checkPackedStructsWithPtrs
  , checkStructContainingPtrWrittenToBinFile
  , checkStructContainingPtrReadFromBinFile
  , checkSizeofStoredIn32bits
  , checkHardCodedStructSizes
  ) where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.IssueTypes
import Analysis.ASTTraversal
import Analysis.TypeChecker
import Analysis.KnownFunctions (allocFns, ioWriteFns, ioReadFns)
import qualified Data.Map as Map

analyzeAlignmentIssues :: CTranslUnit -> [Issue]
analyzeAlignmentIssues ast =
    checkStructsWithMixedPtrNonPtrMembers ast
    ++ checkUnionsContainingPtrAndInts ast
    ++ checkPackedStructsWithPtrs ast
    ++ checkStructContainingPtrWrittenToBinFile ast
    ++ checkStructContainingPtrReadFromBinFile ast
    ++ checkSizeofStoredIn32bits ast
    ++ checkHardCodedStructSizes ast

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Collect all resolved member types from a struct/union CDeclaration.
getMemberTypes :: CDeclaration NodeInfo -> [CType]
getMemberTypes (CDecl specs declrs _) =
    [ resolveType specs derived
    | (Just (CDeclr _ derived _ _ _), _, _) <- declrs ]
getMemberTypes (CStaticAssert {}) = []

-- | True if the attribute list contains @packed@ / @__packed__@.
isPackedStruct :: [CAttribute NodeInfo] -> Bool
isPackedStruct = any isPacked
  where
    isPacked (CAttr (Ident name _ _) _ _) = name `elem` ["packed", "__packed__"]

-- | Apply @f@ to every top-level struct/union definition.
walkStructDefs :: (CStructureUnion NodeInfo -> [Issue]) -> CTranslUnit -> [Issue]
walkStructDefs f (CTranslUnit decls _) = concatMap checkDecl decls
  where
    checkDecl (CDeclExt (CDecl specs _ _)) =
        concatMap checkSpec [s | CTypeSpec s <- specs]
    checkDecl _ = []

    checkSpec (CSUType su _) = f su
    checkSpec _              = []

-- ---------------------------------------------------------------------------
-- Struct-definition checks
-- ---------------------------------------------------------------------------

-- | Flag structs that mix pointer and integer/long members (layout changes on 64-bit).
--   Also flags outer structs whose embedded struct member itself contains a pointer.
checkStructsWithMixedPtrNonPtrMembers :: CTranslUnit -> [Issue]
checkStructsWithMixedPtrNonPtrMembers ast =
    let senv = buildStructEnv ast
    in walkStructDefs (checkSU senv) ast
  where
    checkSU senv (CStruct CStructTag _ (Just members) _ info) =
        let types       = concatMap getMemberTypes members
            hasPtrs     = any isPointer types
                          || any (memberHasNestedPtr senv) types
            hasIntTypes = any (\ t -> isIntType' t || isUIntType t || isLongType' t) types
        in [ createIssue info Warning StructsWithMixedPtrNonPtrMembers
           | hasPtrs && hasIntTypes ]
    checkSU _ _ = []

    memberHasNestedPtr senv (TStruct name) = structHasPointer senv name
    memberHasNestedPtr _ _                 = False

-- | Flag unions that contain both pointer and integer members.
checkUnionsContainingPtrAndInts :: CTranslUnit -> [Issue]
checkUnionsContainingPtrAndInts = walkStructDefs checkSU
  where
    checkSU (CStruct CUnionTag _ (Just members) _ info) =
        let types    = concatMap getMemberTypes members
            hasPtrs  = any isPointer types
            hasInts  = any (\ t -> isIntType' t || isUIntType t || isLongType' t) types
        in [ createIssue info Warning UnionsContainingPtrAndInts
           | hasPtrs && hasInts ]
    checkSU _ = []

-- | Flag packed structs that contain pointer members (pointers lose alignment).
checkPackedStructsWithPtrs :: CTranslUnit -> [Issue]
checkPackedStructsWithPtrs = walkStructDefs checkSU
  where
    checkSU (CStruct _ _ (Just members) attrs info) =
        let types   = concatMap getMemberTypes members
            hasPtrs = any isPointer types
        in [ createIssue info Critical PackedStructsWithPtrs
           | isPackedStruct attrs && hasPtrs ]
    checkSU _ = []

-- ---------------------------------------------------------------------------
-- I/O checks (require StructEnv)
-- ---------------------------------------------------------------------------

-- | Flag fwrite/write calls whose buffer is a pointer to a struct with pointers.
checkStructContainingPtrWrittenToBinFile :: CTranslUnit -> [Issue]
checkStructContainingPtrWrittenToBinFile ast@(CTranslUnit decls _) =
    let senv = buildStructEnv ast
    in concatMap (analyzeDecl (checkWrite senv) Map.empty) decls
  where
    checkWrite senv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ioWriteFns, not (null args) =
            let bufArg  = if fname == "write" && length args >= 2
                          then args !! 1
                          else head args
                bufType = typeOfExpr env bufArg
            in [ createIssue info Critical StructContainingPtrWrittenToBinFile
               | ptrToStructWithPtrs senv bufType ]
    checkWrite _ _ _ = []

-- | Flag fread/read calls whose buffer is a pointer to a struct with pointers.
checkStructContainingPtrReadFromBinFile :: CTranslUnit -> [Issue]
checkStructContainingPtrReadFromBinFile ast@(CTranslUnit decls _) =
    let senv = buildStructEnv ast
    in concatMap (analyzeDecl (checkRead senv) Map.empty) decls
  where
    checkRead senv env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` ioReadFns, not (null args) =
            let bufArg  = if fname == "read" && length args >= 2
                          then args !! 1
                          else head args
                bufType = typeOfExpr env bufArg
            in [ createIssue info Critical StructContainingPtrReadFromBinFile
               | ptrToStructWithPtrs senv bufType ]
    checkRead _ _ _ = []

-- ---------------------------------------------------------------------------
-- Size arithmetic checks
-- ---------------------------------------------------------------------------

-- | Flag assignments where a sizeof result is stored in an int/uint variable.
checkSizeofStoredIn32bits :: CTranslUnit -> [Issue]
checkSizeofStoredIn32bits ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkAssign tenv) Map.empty) decls
  where
    checkAssign tenv env (CAssign CAssignOp lhs rhs info) =
        let lhsType  = resolveTypedef tenv (typeOfExpr env lhs)
            mDeclPos = case lhs of
                CVar (Ident name _ _) _ -> lookupDeclPos env name
                _                       -> Nothing
        in [ createIssueWithDecl info mDeclPos Warning SizeofStoredIn32Bits
           | (isIntType' lhsType || isUIntType lhsType) && isSizeof rhs ]
    checkAssign _ _ _ = []

    isSizeof (CSizeofType _ _) = True
    isSizeof (CSizeofExpr _ _) = True
    isSizeof _                 = False

-- | Flag malloc/calloc calls whose size argument is a raw integer literal
--   (the programmer likely hardcoded the struct size instead of using sizeof).
checkHardCodedStructSizes :: CTranslUnit -> [Issue]
checkHardCodedStructSizes (CTranslUnit decls _) =
    concatMap (analyzeDecl checkExpr Map.empty) decls
  where
    checkExpr _env (CCall (CVar (Ident fname _ _) _) args info)
        | fname `elem` allocFns =
            [ createIssue info Warning HardCodedStructSizes
            | any isLargeLiteral args ]
    checkExpr _ _ = []

    isLargeLiteral (CConst (CIntConst n _)) = getCInteger n > 8
    isLargeLiteral _                           = False