module Analysis.UtilTypes where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Data.Char (toUpper)
import qualified Data.Map as Map

type VarContext = Map.Map String Bool

data Severity = Critical | Warning | Low deriving (Show, Eq)

data IssueTag
    -- Alignment Issues
    = StructContainingPtrWrittenToBinFile
    | StrucContainingPtrReadFromBinFile
    | StructsWithMixedPtrNonPtrMembers
    | UnionsContainingPtrAndInts
    | PackedStructsWithPtrs
    | SizeofStoredin32bits
    | HardCodedStructSizes
    -- Bit Manipulation Issues
    | PackingPtrsWithFlagsInInt
    | BitShiftsOnPtr
    | ExtractingPtrBitsIn32BitVar
    -- Comparison Issues
    | LoopCounterAsIntWhenIteratingOverPtrArrays
    | PtrComparisonWithIntConsts
    | UsingIntForFileOffsets
    -- Constants and Literal Issues
    | MagicValuesUsed
    | BitMaskingAssuming32bitPts
    | HardCodedAddressValues
    | ConstantsUsedForSizeCalcs
    -- Format String Issues
    | DUsedWithSizet
    | UUsedWithSizet
    | XUsedWithSizet
    | DUsedWithPtrdifft
    | UUsedWithPtrdifft
    | DUsedWithPtr
    | UUsedWithPtr
    | XUsedWithPtr
    | LuUsedForPtrSizedVals
    | LdUsedWithLongAssuming64bits
    -- Function Signature Issues
    | FnsReturnPtrAsInt
    | FnsReturnPtrAsLong
    | FnsParamDeclaredAsIntTakesPtr
    | VaargUsingWrongTypesForPtrArgs
    -- Memory Allocation Issues
    | AllocationSizeCalculationsMayOverflow
    | MallocWithoutOverflowChecking
    | UsingIntToStoreAllocationSizes
    -- Platform Specific Issues
    | InlineAsmWithx86Instructions
    | AsmBlocks
    | HandleTypesCastToInt
    | X86SpecificCompilerIntrinsics
    | AssumptionsAboutRegSizes
    -- Pointer Math Issues
    | PtrDiffStoredAs32bit
    | PointerAddOverflow
    | PtrSubUnderflow
    | ArrayIndexingIntInArrayOver2tothe31size
    -- Serialization Issues
    | WritingPtrDirectToFile
    | WritingPtrContrainingStructsToFiles
    | SendingPtrsOverNetwork
    | PtrInMemoryMappedFiles
    | PtrInSharedMemory
    -- Type Size Issues
    | CastPointerToInt
    | CastPointerToUInt
    | CastIntToPointer
    | CastLongToPointer
    | SizeOfIntIsVoid
    | SizeOfLongIsVoid
    | UsingIntAsSizet
    | UsingIntAsPtrdifft
    | UsingUIntAsMemSize
    deriving (Show, Eq)

data Category 
    = AlignmentIssue
    | BitManipulationIssue
    | ComparisonIssue
    | ConstantLiteralsIssue
    | FormatStringsIssue
    | FunctionSignaturesIssue
    | MemoryAllocationIssue
    | PlatformSpecificsIssue
    | PointerMathIssue
    | SerializationIssue
    | TypeSizeIssue
    deriving (Show,Eq)


getCategory :: IssueTag -> Category
getCategory tag = case tag of 
    -- Alignment Issues
    StructContainingPtrWrittenToBinFile -> AlignmentIssue
    StrucContainingPtrReadFromBinFile -> AlignmentIssue
    StructsWithMixedPtrNonPtrMembers -> AlignmentIssue
    UnionsContainingPtrAndInts -> AlignmentIssue
    PackedStructsWithPtrs -> AlignmentIssue
    SizeofStoredin32bits -> AlignmentIssue
    HardCodedStructSizes -> AlignmentIssue
    -- Bit Manipulation Issues
    PackingPtrsWithFlagsInInt -> BitManipulationIssue
    BitShiftsOnPtr -> BitManipulationIssue
    ExtractingPtrBitsIn32BitVar -> BitManipulationIssue
    -- Comparison Issues
    LoopCounterAsIntWhenIteratingOverPtrArrays -> ComparisonIssue
    PtrComparisonWithIntConsts -> ComparisonIssue
    UsingIntForFileOffsets -> ComparisonIssue
    -- Constants and Literal Issues
    MagicValuesUsed -> ConstantLiteralsIssue
    BitMaskingAssuming32bitPts -> ConstantLiteralsIssue
    HardCodedAddressValues -> ConstantLiteralsIssue
    ConstantsUsedForSizeCalcs -> ConstantLiteralsIssue
    -- Format String Issues
    DUsedWithSizet -> FormatStringsIssue
    UUsedWithSizet -> FormatStringsIssue
    XUsedWithSizet -> FormatStringsIssue
    DUsedWithPtrdifft -> FormatStringsIssue
    UUsedWithPtrdifft -> FormatStringsIssue
    DUsedWithPtr -> FormatStringsIssue
    UUsedWithPtr -> FormatStringsIssue
    XUsedWithPtr -> FormatStringsIssue
    LuUsedForPtrSizedVals -> FormatStringsIssue
    LdUsedWithLongAssuming64bits -> FormatStringsIssue
    -- Function Signature Issues
    FnsReturnPtrAsInt -> FunctionSignaturesIssue
    FnsReturnPtrAsLong -> FunctionSignaturesIssue
    FnsParamDeclaredAsIntTakesPtr -> FunctionSignaturesIssue
    VaargUsingWrongTypesForPtrArgs -> FunctionSignaturesIssue
    -- Memory Allocation Issues
    AllocationSizeCalculationsMayOverflow -> MemoryAllocationIssue
    MallocWithoutOverflowChecking -> MemoryAllocationIssue
    UsingIntToStoreAllocationSizes -> MemoryAllocationIssue
    -- Platform Specific Issues
    InlineAsmWithx86Instructions -> PlatformSpecificsIssue
    AsmBlocks -> PlatformSpecificsIssue
    HandleTypesCastToInt -> PlatformSpecificsIssue
    X86SpecificCompilerIntrinsics -> PlatformSpecificsIssue
    AssumptionsAboutRegSizes -> PlatformSpecificsIssue
    -- Pointer Math Issues
    PtrDiffStoredAs32bit -> PointerMathIssue
    PointerAddOverflow -> PointerMathIssue
    PtrSubUnderflow -> PointerMathIssue
    ArrayIndexingIntInArrayOver2tothe31size -> PointerMathIssue
    -- Serialization Issues
    WritingPtrDirectToFile -> SerializationIssue
    WritingPtrContrainingStructsToFiles -> SerializationIssue
    SendingPtrsOverNetwork -> SerializationIssue
    PtrInMemoryMappedFiles -> SerializationIssue
    PtrInSharedMemory -> SerializationIssue
    -- Type Size Issues
    CastPointerToInt -> TypeSizeIssue
    CastPointerToUInt -> TypeSizeIssue
    CastIntToPointer -> TypeSizeIssue
    CastLongToPointer -> TypeSizeIssue
    SizeOfIntIsVoid -> TypeSizeIssue
    SizeOfLongIsVoid -> TypeSizeIssue
    UsingIntAsSizet -> TypeSizeIssue
    UsingIntAsPtrdifft -> TypeSizeIssue
    UsingUIntAsMemSize -> TypeSizeIssue


data Issue = Issue
    { issuePos :: NodeInfo
    , issueSeverity :: Severity
    , issueType :: IssueTag
    , catagory :: Category
    , code :: Maybe String
    }

instance Show Issue where
    show i = show (catagory i) ++ ": " ++ show (issueType i) 
        ++ " [" ++ map toUpper (show (issueSeverity i)) ++ "]" 
        ++ " at " ++ show (issuePos i)
        ++ "\nCode: " ++ show (code i)

getCode :: CTranslUnit -> NodeInfo -> Maybe String
getCode (CTranslUnit decls _) nodeInfo = 
    case extractCodeSnippet <$> findDeclAtPosition decls nodeInfo of
        Just snippet -> Just snippet
        Nothing -> Nothing

findDeclAtPosition :: [CExternalDeclaration NodeInfo] -> NodeInfo -> Maybe (CExternalDeclaration NodeInfo)
findDeclAtPosition decls targetInfo = 
    case filter (samePosition targetInfo) decls of
        (decl:_) -> Just decl
        _ -> Nothing
  where
    samePosition target (CFDefExt (CFunDef _ _ _ _ info)) = 
        target == info
    samePosition target (CDeclExt (CDecl _ _ info)) = 
        target == info
    samePosition _ _ = False

extractCodeSnippet :: CExternalDeclaration a -> String
extractCodeSnippet (CFDefExt (CFunDef _ _ declrs _ _)) =
    case declrs of
        (CDecl _ declrs' _:_) ->
            case declrs' of
                ((Just (CDeclr (Just (Ident fname _ _)) _ _ _ _), _, _):_) -> 
                    "In function: " ++ fname
                _ -> "Unknown function"
        _ -> "Unknown function"
extractCodeSnippet (CDeclExt (CDecl _ declrs _)) =
    case declrs of
        ((Just (CDeclr (Just (Ident vname _ _)) _ _ _ _), _, _):_) -> 
            "Variable: " ++ vname
        _ -> "Unknown declaration"
extractCodeSnippet _ = "Unknown code"

createIssue :: CTranslUnit -> NodeInfo -> Severity -> IssueTag -> Issue
createIssue _ pos sev tag = Issue pos sev tag (getCategory tag) (getCode undefined pos)


isIntType :: CDeclaration a -> Bool
isIntType (CDecl [CTypeSpec (CIntType _)] _ _) = True
isIntType (CDecl [CTypeSpec (CUnsigType _), CTypeSpec (CIntType _)] _ _) = True
isIntType (CDecl [CTypeSpec (CIntType _), CTypeSpec (CUnsigType _)] _ _) = True
isIntType _ = False

exprMightBePointerWithContext :: VarContext -> CExpression a -> Bool
exprMightBePointerWithContext ctx expr = case expr of
    CVar (Ident name _ _) _ -> Map.findWithDefault False name ctx
    CUnary CIndOp _ _ -> True
    CUnary CAdrOp _ _ -> True
    CCast decl _ _ -> isPtrDecl decl
    _ -> False
  where
    isPtrDecl (CDecl _ declrs _) = any (containsPtr . fst3) declrs
    fst3 (a, _, _) = a
    
    containsPtr Nothing = False
    containsPtr (Just decl) = hasPtr decl
    
    hasPtr (CDeclr _ derived _ _ _) = any isPtrDerived derived
    
    isPtrDerived (CPtrDeclr _ _) = True
    isPtrDerived _ = False

analyzeDecl :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CExternalDeclaration a -> [Issue]
analyzeDecl f ctx (CFDefExt funDef) = analyzeFunDef f ctx funDef
analyzeDecl _ _ (CDeclExt _) = []
analyzeDecl _ _ _ = []

analyzeFunDef :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CFunctionDef a -> [Issue]
analyzeFunDef f ctx (CFunDef _ _ _ stmt _) = analyzeStmt f ctx stmt

analyzeStmt :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CStatement a -> [Issue]
analyzeStmt f ctx stmt = case stmt of
    CExpr (Just expr) _ -> f ctx expr
    CCompound _ items _ -> 
        let ctx' = buildContext items ctx
        in concatMap (analyzeCompoundBlockItem f ctx') items
    CIf _ thenStmt maybeElse _ -> 
        analyzeStmt f ctx thenStmt ++ maybe [] (analyzeStmt f ctx) maybeElse
    CWhile _ body _ _ -> analyzeStmt f ctx body
    CFor _ _ _ body _ -> analyzeStmt f ctx body
    _ -> []

analyzeCompoundBlockItem :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CCompoundBlockItem a -> [Issue]
analyzeCompoundBlockItem f ctx (CBlockStmt stmt) = analyzeStmt f ctx stmt
analyzeCompoundBlockItem f ctx (CBlockDecl decl) = analyzeDeclration f ctx decl
analyzeCompoundBlockItem _ _ _ = []

analyzeDeclration :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CDeclaration a -> [Issue]
analyzeDeclration f ctx (CDecl _ declrs _) = concatMap (analyzeDeclr f ctx) declrs
  where
    analyzeDeclr f ctx (_, maybeInit, maybeExpr) = 
        case maybeExpr of
            Just expr -> f ctx expr
            Nothing -> 
                case maybeInit of
                    Just init -> analyzeInit f ctx init
                    Nothing -> []

analyzeInit :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> CInitializer a -> [Issue]
analyzeInit f ctx (CInitExpr expr _) = f ctx expr
analyzeInit f ctx (CInitList initList _) = 
    let items = initList
    in concatMap (\(_, initializer) -> analyzeInit f ctx initializer) items
analyzeInit _ _ _ = []

analyzeInitItem :: (VarContext -> CExpression a -> [Issue]) -> VarContext -> (CPartDesignator a, CInitializer a) -> [Issue]
analyzeInitItem f ctx (_, initializer) = analyzeInit f ctx initializer

buildContext :: [CCompoundBlockItem a] -> VarContext -> VarContext
buildContext items ctx = foldl addItem ctx items
  where
    addItem context (CBlockDecl (CDecl specs declrs _)) = 
        foldl (addDeclr specs) context declrs
    addItem context _ = context
    
    addDeclr specs context (Just (CDeclr (Just (Ident name _ _)) derived _ _ _), _, _) =
        Map.insert name (checkPtr derived) context
    addDeclr _ context _ = context
    
    checkPtr derived = any isPtrDerived derived
    isPtrDerived (CPtrDeclr _ _) = True
    isPtrDerived _ = False