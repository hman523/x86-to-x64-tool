module Analysis.UtilTypes where

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Data.Char (toUpper)
import qualified Data.Map as Map
import Analysis.TypeChecker (TypeEnv, CType(..), collectDecl, resolveType)

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
    { issuePos      :: NodeInfo
    , issueSeverity :: Severity
    , issueType     :: IssueTag
    , catagory      :: Category
    }

instance Show Issue where
    show i = show (catagory i) ++ ": " ++ show (issueType i) 
        ++ " [" ++ map toUpper (show (issueSeverity i)) ++ "]" 
        ++ " at " ++ show (issuePos i)

createIssue :: NodeInfo -> Severity -> IssueTag -> Issue
createIssue pos sev tag = Issue pos sev tag (getCategory tag)
-- ---------------------------------------------------------------------------
-- AST traversal helpers
-- ---------------------------------------------------------------------------

analyzeDecl :: (TypeEnv -> CExpression a -> [Issue]) 
            -> TypeEnv 
            -> CExternalDeclaration a 
            -> [Issue]
analyzeDecl f ctx (CFDefExt funDef) = analyzeFunDef f ctx funDef
analyzeDecl _ _ (CDeclExt _)        = []
analyzeDecl _ _ _                   = []

analyzeFunDef :: (TypeEnv -> CExpression a -> [Issue]) 
              -> TypeEnv 
              -> CFunctionDef a 
              -> [Issue]
analyzeFunDef f ctx (CFunDef _ _ _ stmt _) = analyzeStmt f ctx stmt

analyzeStmt :: (TypeEnv -> CExpression a -> [Issue]) 
            -> TypeEnv 
            -> CStatement a 
            -> [Issue]
analyzeStmt f env stmt = case stmt of
    CExpr (Just expr) _    -> walkExpr f env expr
    CCompound _ items _    ->
        let (issues, _) = foldl (stepItem f) ([], env) items
        in issues
    CIf cond thenS elseS _ ->
        walkExpr f env cond
        ++ analyzeStmt f env thenS
        ++ maybe [] (analyzeStmt f env) elseS
    CWhile cond body _ _   -> walkExpr f env cond ++ analyzeStmt f env body
    CFor init cond step body _ ->
        let initIssues = case init of
                Left (Just expr) -> walkExpr f env expr
                Left Nothing     -> []
                Right decl       -> analyzeDeclration f env decl
            env'       = case init of
                Right decl -> collectDecl decl env
                _          -> env
            condIssues = maybe [] (walkExpr f env') cond
            stepIssues = maybe [] (walkExpr f env') step
            bodyIssues = analyzeStmt f env' body
        in initIssues ++ condIssues ++ stepIssues ++ bodyIssues
    CReturn (Just expr) _  -> walkExpr f env expr
    _                      -> []

-- | Apply f to an expression and all of its sub-expressions recursively.
walkExpr :: (TypeEnv -> CExpression a -> [Issue])
         -> TypeEnv
         -> CExpression a
         -> [Issue]
walkExpr f env expr = f env expr ++ concatMap (walkExpr f env) (childExprs expr)

-- | Direct child expressions of a C expression node.
childExprs :: CExpression a -> [CExpression a]
childExprs expr = case expr of
    CAssign _ l r _    -> [l, r]
    CBinary _ l r _    -> [l, r]
    CUnary  _ e _      -> [e]
    CCast   _ e _      -> [e]
    CCall fn args _    -> fn : args
    CMember e _ _ _    -> [e]
    CIndex  e1 e2 _    -> [e1, e2]
    CCond c t e _      -> maybe [c, e] (\t' -> [c, t', e]) t
    CComma  es _       -> es
    _                  -> []

stepItem :: (TypeEnv -> CExpression a -> [Issue])
         -> ([Issue], TypeEnv)
         -> CCompoundBlockItem a
         -> ([Issue], TypeEnv)
stepItem f (issues, env) item = case item of
    CBlockDecl decl ->
        let env'   = collectDecl decl env
            newIss = analyzeDeclration f env decl
        in (issues ++ newIss, env')
    CBlockStmt stmt ->
        (issues ++ analyzeStmt f env stmt, env)
    _ -> (issues, env)

analyzeDeclration :: (TypeEnv -> CExpression a -> [Issue]) 
                  -> TypeEnv 
                  -> CDeclaration a 
                  -> [Issue]
analyzeDeclration f env (CDecl _ declrs _) = concatMap (analyzeDeclr f env) declrs
  where
    analyzeDeclr g ctx (_, maybeInit, maybeExpr) =
        case maybeExpr of
            Just expr -> g ctx expr
            Nothing   ->
                case maybeInit of
                    Just ini -> analyzeInit g ctx ini
                    Nothing  -> []

analyzeInit :: (TypeEnv -> CExpression a -> [Issue]) 
            -> TypeEnv 
            -> CInitializer a 
            -> [Issue]
analyzeInit f env (CInitExpr expr _)     = walkExpr f env expr
analyzeInit f env (CInitList initList _) =
    concatMap (\(_, ini) -> analyzeInit f env ini) initList
analyzeInit _ _ _                        = []

