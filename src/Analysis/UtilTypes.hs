module Analysis.UtilTypes where

data Severity = Critical | Warning | Low
deriving (Show, Eq)

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
    MagicValuesUsed -> ConstantLiteralIssue
    BitMaskingAssuming32bitPts -> ConstantLiteralIssue
    HardCodedAddressValues -> ConstantLiteralIssue
    ConstantsUsedForSizeCalcs -> ConstantLiteralIssue
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


data Issue = 
    { position :: NodeInfo
    , issueSeverity :: Severity
    , issueType :: IssueTag
    , catagory :: Category
    , code :: Maybe String
    }

instance Show Issue where
    show i = show $ category i ++ ": " ++ show $ issueType i 
        ++ "[" ++ toUpper $ show $ issueSeverity i ++ "]" 
        ++ " at " ++ show $ position i 
        ++ "\nCode: " ++ show $ code i

getCode :: CTranslUnit -> NodeInfo -> Maybe String


createIssue :: CTranslUnit -> NodeInfo -> Severity -> IssueTag -> Issue
createIssue code pos sev tag = Issue pos sev tag (getCategory tag) (getCode code pos)