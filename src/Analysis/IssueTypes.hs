module Analysis.IssueTypes
  ( Severity(..)
  , IssueTag(..)
  , Category(..)
  , Issue(..)
  , getCategory
  , createIssue
  , createIssueWithDecl
  , issueType
  , category
  , issueSeverity
  , issuePos
  , issueDeclPos
  , prettyPrintIssues
  ) where

import Language.C.Data.Node (NodeInfo)
import Data.Char (toUpper)
import Language.C.Data.Position (posOf, posFile, posRow)

data Severity = Critical | Warning deriving (Show, Eq)

data IssueTag
    -- Alignment Issues
    = StructContainingPtrWrittenToBinFile
    | StructContainingPtrReadFromBinFile
    | StructsWithMixedPtrNonPtrMembers
    | UnionsContainingPtrAndInts
    | PackedStructsWithPtrs
    | SizeofStoredIn32Bits
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
    | FnsReturnPtrAsUInt
    | FnsReturnPtrAsLong
    | FnsParamDeclaredAsIntTakesPtr
    | FnsParamDeclaredAsUIntTakesPtr
    | VaargUsingWrongTypesForPtrArgs
    | VaargUsingWrongTypesForPtrArgsUInt
    -- Memory Allocation Issues
    | AllocationSizeCalcsMayOverflow
    | MallocWithoutOverflowChecking
    | UsingIntToStoreAllocationSizes
    -- Platform Specific Issues
    | InlineAsmWithx86Instructions
    | AsmBlocks
    | HandleTypesCastToInt
    | HandleTypesCastToUInt
    | X86SpecificCompilerIntrinsics
    | AssumptionsAboutRegSizes
    -- Pointer Math Issues
    | PtrDiffStoredAs32bit
    | PointerAddOverflow
    | PtrSubUnderflow
    | ArrayIndexingIntInArrayOver2tothe31size
    -- Serialization Issues
    | WritingPtrDirectToFile
    | WritingPtrContainingStructsToFiles
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
    deriving (Eq)

instance Show Category where
    show cat = case cat of
        AlignmentIssue          -> "Alignment Issue"
        BitManipulationIssue    -> "Bit Manipulation Issue"
        ComparisonIssue         -> "Comparison Issue"
        ConstantLiteralsIssue   -> "Constant/Literal Issue"
        FormatStringsIssue      -> "Format String Issue"
        FunctionSignaturesIssue -> "Function Signature Issue"
        MemoryAllocationIssue   -> "Memory Allocation Issue"
        PlatformSpecificsIssue  -> "Platform Specifics Issue"
        PointerMathIssue        -> "Pointer Math Issue"
        SerializationIssue      -> "Serialization Issue"
        TypeSizeIssue           -> "Type Size Issue"

getCategory :: IssueTag -> Category
getCategory tag = case tag of 
    -- Alignment Issues
    StructContainingPtrWrittenToBinFile -> AlignmentIssue
    StructContainingPtrReadFromBinFile -> AlignmentIssue
    StructsWithMixedPtrNonPtrMembers -> AlignmentIssue
    UnionsContainingPtrAndInts -> AlignmentIssue
    PackedStructsWithPtrs -> AlignmentIssue
    SizeofStoredIn32Bits -> AlignmentIssue
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
    FnsReturnPtrAsUInt -> FunctionSignaturesIssue
    FnsReturnPtrAsLong -> FunctionSignaturesIssue
    FnsParamDeclaredAsIntTakesPtr -> FunctionSignaturesIssue
    FnsParamDeclaredAsUIntTakesPtr -> FunctionSignaturesIssue
    VaargUsingWrongTypesForPtrArgs -> FunctionSignaturesIssue
    VaargUsingWrongTypesForPtrArgsUInt -> FunctionSignaturesIssue
    -- Memory Allocation Issues
    AllocationSizeCalcsMayOverflow -> MemoryAllocationIssue
    MallocWithoutOverflowChecking -> MemoryAllocationIssue
    UsingIntToStoreAllocationSizes -> MemoryAllocationIssue
    -- Platform Specific Issues
    InlineAsmWithx86Instructions -> PlatformSpecificsIssue
    AsmBlocks -> PlatformSpecificsIssue
    HandleTypesCastToInt -> PlatformSpecificsIssue
    HandleTypesCastToUInt -> PlatformSpecificsIssue
    X86SpecificCompilerIntrinsics -> PlatformSpecificsIssue
    AssumptionsAboutRegSizes -> PlatformSpecificsIssue
    -- Pointer Math Issues
    PtrDiffStoredAs32bit -> PointerMathIssue
    PointerAddOverflow -> PointerMathIssue
    PtrSubUnderflow -> PointerMathIssue
    ArrayIndexingIntInArrayOver2tothe31size -> PointerMathIssue
    -- Serialization Issues
    WritingPtrDirectToFile -> SerializationIssue
    WritingPtrContainingStructsToFiles -> SerializationIssue
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
    , issueDeclPos  :: Maybe NodeInfo
    , issueSeverity :: Severity
    , issueType     :: IssueTag
    , category      :: Category
    }

instance Show Issue where
    show i = show (category i) ++ ": " ++ show (issueType i) 
        ++ " [" ++ map toUpper (show (issueSeverity i)) ++ "]" 
        ++ " at " ++ show (issuePos i)

-- ---------------------------------------------------------------------------
-- ANSI colour helpers
-- ---------------------------------------------------------------------------

ansi :: Bool -> String -> String -> String
ansi False _    s = s
ansi True  code s = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"

sevCode :: Severity -> String
sevCode Critical = "1;31"   -- bold red
sevCode Warning  = "1;33"   -- bold yellow

catCode :: Category -> String
catCode AlignmentIssue         = "35"    -- magenta
catCode BitManipulationIssue   = "34"    -- blue
catCode ComparisonIssue        = "36"    -- cyan
catCode ConstantLiteralsIssue  = "33"    -- yellow
catCode FormatStringsIssue     = "94"    -- bright blue
catCode FunctionSignaturesIssue = "95"  -- bright magenta
catCode MemoryAllocationIssue  = "91"    -- bright red
catCode PlatformSpecificsIssue = "96"    -- bright cyan
catCode PointerMathIssue       = "32"    -- green
catCode SerializationIssue     = "93"    -- bright yellow
catCode TypeSizeIssue          = "97"    -- bright white

-- ---------------------------------------------------------------------------
-- Pretty printer
-- ---------------------------------------------------------------------------

wrapText :: Int -> Int -> String -> String
wrapText availableWidth contIndent text = go (words text) 0 ""
  where
    indentStr = replicate contIndent ' '
    go []     _   acc = acc
    go (w:ws) col acc
        | col == 0                             = go ws (length w)             w
        | col + 1 + length w <= availableWidth = go ws (col + 1 + length w)   (acc ++ " " ++ w)
        | otherwise                            = go ws (length w)             (acc ++ "\n" ++ indentStr ++ w)

prettyPrintIssues :: Bool -> Bool -> Int -> [Issue] -> String
prettyPrintIssues verbose useColor termWidth issues =
    let total    = length issues
        numWidth = length (show total)   -- digits for the largest index
        sevWidth = 10                    -- width of "[CRITICAL]"

        fmtLoc x = posFile (posOf (issuePos x))
                   ++ ":" ++ show (posRow (posOf (issuePos x))) ++ ":"
        locWidth = maximum (map (length . fmtLoc) issues)

        -- indent for explanation lines: "NN) -> "
        explanIndent = numWidth + 5
        availW       = max 20 (termWidth - explanIndent)

        padLeft  n s = replicate (max 0 (n - length s)) ' ' ++ s
        padRight n s = s ++ replicate (max 0 (n - length s)) ' '

        fmt (i, x) =
            let sev    = issueSeverity x
                cat    = category x
                num    = padLeft numWidth (show i) ++ ") "
                sevStr = ansi useColor (sevCode sev)
                             (padRight sevWidth ("[" ++ map toUpper (show sev) ++ "]"))
                loc    = ansi useColor "1;36"
                             (padRight locWidth (fmtLoc x))
                tagStr = "\n" ++ replicate (numWidth + 2) ' ' 
                         ++ ansi useColor (catCode cat) (show (issueType x))
                catStr = ansi useColor (catCode cat) ("(" ++ show cat ++ ")")
                explanation
                    | verbose   = "\n" ++ replicate (numWidth + 2) ' '
                                  ++ "-> "
                                  ++ wrapText availW explanIndent (describeIssue (issueType x))
                                  ++ declNote x
                                  ++ "\n"
                    | otherwise = ""
                declNote issue = case issueDeclPos issue of
                    Nothing -> ""
                    Just ni -> "\n" ++ replicate explanIndent ' '
                               ++ ansi useColor "1;36"
                                   ("Declared at " ++ posFile (posOf ni)
                                    ++ ":" ++ show (posRow (posOf ni)) ++ ".")
            in num ++ sevStr ++ " " ++ loc ++ " " ++ tagStr ++ " " ++ catStr ++ explanation
    in unlines $ zipWith (curry fmt) [(1::Int)..] issues

createIssue :: NodeInfo -> Severity -> IssueTag -> Issue
createIssue pos sev tag = Issue pos Nothing sev tag (getCategory tag)

createIssueWithDecl :: NodeInfo -> Maybe NodeInfo -> Severity -> IssueTag -> Issue
createIssueWithDecl pos mDeclPos sev tag = Issue pos mDeclPos sev tag (getCategory tag)

-- | One-sentence explanation of why each issue matters for x86-to-x64 migration.
describeIssue :: IssueTag -> String
describeIssue tag = case tag of
    -- Alignment
    StructContainingPtrWrittenToBinFile      -> "Pointer members change byte width on 64-bit, making the binary file format incompatible across architectures."
    StructContainingPtrReadFromBinFile        -> "Reading a struct with pointer members from a binary file assumes a fixed pointer size that differs between 32-bit and 64-bit."
    StructsWithMixedPtrNonPtrMembers         -> "Mixing pointer and non-pointer members can cause unexpected padding differences between 32-bit and 64-bit due to pointer alignment requirements."
    UnionsContainingPtrAndInts               -> "A union over a pointer and an integer has different sizes on 32-bit vs 64-bit, causing incorrect integer values when the pointer is stored."
    PackedStructsWithPtrs                    -> "Packed structs suppress alignment padding, which can cause unaligned pointer access on 64-bit architectures."
    SizeofStoredIn32Bits                     -> "sizeof returns size_t, which is 64-bit on a 64-bit system; truncating it to a 32-bit int silently discards the upper 32 bits."
    HardCodedStructSizes                     -> "Hardcoded byte sizes for struct allocations break when pointer members change width on a 64-bit target."
    -- Bit Manipulation
    PackingPtrsWithFlagsInInt                -> "Packing a pointer and flags into a 32-bit int loses the upper 32 bits of a 64-bit pointer."
    BitShiftsOnPtr                           -> "Bit-shifting a pointer is only safe if all bits fit within the shift target type, which is not true for 64-bit pointers."
    ExtractingPtrBitsIn32BitVar              -> "Extracting bits of a 64-bit pointer into a 32-bit variable silently discards the upper half of the address."
    -- Comparison
    LoopCounterAsIntWhenIteratingOverPtrArrays -> "A 32-bit int loop counter cannot represent pointer differences or array sizes larger than 2^31 elements on 64-bit systems."
    PtrComparisonWithIntConsts               -> "Comparing a pointer to a hardcoded integer constant assumes a specific address-space range that is not valid on 64-bit."
    UsingIntForFileOffsets                   -> "File offsets can exceed 2^31 bytes on 64-bit systems; using int instead of off_t or long truncates large offsets."
    -- Constants and Literals
    MagicValuesUsed                          -> "Using the literal 4 or 8 as an allocation size assumes a specific pointer width that will be wrong when the word size changes."
    BitMaskingAssuming32bitPts               -> "Masking with 0xFFFFFFFF silently discards the upper 32 bits of a 64-bit pointer."
    HardCodedAddressValues                   -> "Hardcoded memory addresses are meaningless on 64-bit systems where the address-space layout differs entirely."
    ConstantsUsedForSizeCalcs                -> "Assigning an integer literal to a size_t variable ignores that size_t is 64-bit on 64-bit systems and may not hold values above 2^31."
    -- Format Strings
    DUsedWithSizet                           -> "%d prints a 32-bit int; size_t is 64-bit on 64-bit systems, so use %zu or %lu instead."
    UUsedWithSizet                           -> "%u prints an unsigned 32-bit int; size_t is 64-bit on 64-bit systems, so use %zu instead."
    XUsedWithSizet                           -> "%x prints a 32-bit hex value; size_t is 64-bit on 64-bit systems, so use %zx instead."
    DUsedWithPtrdifft                        -> "%d prints a 32-bit int; ptrdiff_t is 64-bit on 64-bit systems, so use %td instead."
    UUsedWithPtrdifft                        -> "%u prints an unsigned 32-bit int; ptrdiff_t is 64-bit on 64-bit systems, so use %td instead."
    DUsedWithPtr                             -> "%d prints a 32-bit int and will truncate a 64-bit pointer; use %p or cast to uintptr_t with %zu."
    UUsedWithPtr                             -> "%u prints an unsigned 32-bit int and will truncate a 64-bit pointer; use %p instead."
    XUsedWithPtr                             -> "%x prints a 32-bit hex value and will truncate a 64-bit pointer; use %p or %lx instead."
    LuUsedForPtrSizedVals                    -> "%lu assumes long is 64-bit, but on 64-bit Windows long is 32-bit; use %zu with size_t for portability."
    LdUsedWithLongAssuming64bits             -> "%ld assumes long is 64-bit, but on 64-bit Windows long is 32-bit; use PRId64 for portable 64-bit printing."
    -- Function Signatures
    FnsReturnPtrAsInt                        -> "Returning a pointer as a 32-bit int truncates the upper 32 bits of the address on 64-bit platforms."
    FnsReturnPtrAsUInt                       -> "Returning a pointer as a 32-bit unsigned int truncates the upper 32 bits of the address on 64-bit platforms."
    FnsReturnPtrAsLong                       -> "Returning a pointer as long is not portable; long is 32 bits on Windows, so use intptr_t instead."
    FnsParamDeclaredAsIntTakesPtr            -> "Declaring a parameter as int but assigning a pointer to it truncates the upper 32 bits of the address on 64-bit platforms."
    FnsParamDeclaredAsUIntTakesPtr           -> "Declaring a parameter as unsigned int but assigning a pointer to it truncates the upper 32 bits of the address on 64-bit platforms."
    VaargUsingWrongTypesForPtrArgs           -> "Extracting a pointer argument via va_arg as int silently truncates the 64-bit pointer address."
    VaargUsingWrongTypesForPtrArgsUInt       -> "Extracting a pointer argument via va_arg as unsigned int silently truncates the 64-bit pointer address."
    -- Memory Allocation
    AllocationSizeCalcsMayOverflow    -> "Multiplying two 32-bit ints to compute an allocation size can overflow before the result is widened to size_t."
    MallocWithoutOverflowChecking            -> "Adding two 32-bit ints as a malloc size argument can overflow to a small value, causing an under-allocation."
    UsingIntToStoreAllocationSizes           -> "sizeof returns size_t which is 64-bit on 64-bit systems; storing it in an int silently truncates values above 2 GB."
    -- Platform Specifics
    InlineAsmWithx86Instructions             -> "Inline assembly using x86 register names (e.g., eax, ebx) will not compile or behave correctly on x86-64 or other architectures."
    AsmBlocks                                -> "Inline assembly blocks are architecture-specific and must be rewritten or conditionally compiled for x86-64."
    HandleTypesCastToInt                     -> "Windows HANDLE is a pointer-sized type; casting it to int truncates the upper 32 bits on 64-bit Windows."
    HandleTypesCastToUInt                    -> "Windows HANDLE is a pointer-sized type; casting it to unsigned int truncates the upper 32 bits on 64-bit Windows."
    X86SpecificCompilerIntrinsics            -> "Compiler intrinsics with _mm_ prefixes are x86/SSE-specific and require porting or conditional compilation for other architectures."
    AssumptionsAboutRegSizes                 -> "Assuming sizeof(int) == 4 bakes in a platform-specific value that should be verified at compile time via a typedef or static_assert."
    -- Pointer Math
    PtrDiffStoredAs32bit                     -> "Pointer subtraction yields ptrdiff_t, which is 64-bit on 64-bit systems; storing it in a 32-bit int truncates large differences."
    PointerAddOverflow                       -> "Adding a 32-bit int offset to a pointer can overflow the offset before it is widened to ptrdiff_t, producing a wrong address."
    PtrSubUnderflow                          -> "Subtracting an unsigned int from a pointer risks underflow when the unsigned value is promoted during pointer arithmetic."
    ArrayIndexingIntInArrayOver2tothe31size  -> "Using a 32-bit int as an array index limits addressable elements to 2^31; arrays larger than 2 GB require a 64-bit index type."
    -- Serialization
    WritingPtrDirectToFile                   -> "Writing a raw pointer value to a file embeds a 32-bit or 64-bit address that will be invalid when read back on a different architecture."
    WritingPtrContainingStructsToFiles      -> "Writing a struct containing pointer members to a binary file embeds pointer-width-dependent values that differ between 32-bit and 64-bit."
    SendingPtrsOverNetwork                   -> "Sending a pointer or a struct containing pointers over the network transmits an address that is meaningless on any other machine or architecture."
    PtrInMemoryMappedFiles                   -> "Storing a pointer in a memory-mapped file embeds an address that is invalid when the file is mapped at a different base address or on a different word-size system."
    PtrInSharedMemory                        -> "Storing pointers in shared memory is unsafe across processes or systems with different address spaces or pointer widths."
    -- Type Size
    CastPointerToInt                         -> "Casting a pointer to int truncates the upper 32 bits of a 64-bit pointer, producing an invalid value."
    CastPointerToUInt                        -> "Casting a pointer to unsigned int truncates the upper 32 bits of a 64-bit pointer."
    CastIntToPointer                         -> "Casting an int to a pointer sign-extends the 32-bit value to 64 bits, which may not produce the intended address."
    CastLongToPointer                        -> "Casting long to a pointer is not portable; on 64-bit Windows, long is 32-bit (LLP64), so the cast can truncate the address."
    SizeOfIntIsVoid                          -> "sizeof(int) differs from sizeof(void*) on 64-bit platforms; code that assumes they are equal will compute wrong sizes."
    SizeOfLongIsVoid                         -> "sizeof(long) differs from sizeof(void*) on 64-bit Windows (LLP64); portability requires ptrdiff_t or intptr_t."
    UsingIntAsSizet                          -> "Using int where size_t is needed loses the upper 32 bits on 64-bit systems and causes incorrect size computations for objects larger than 2 GB."
    UsingIntAsPtrdifft                       -> "Using int where ptrdiff_t is needed truncates pointer differences that exceed 2^31 on 64-bit systems."
    UsingUIntAsMemSize                       -> "unsigned int is only 32-bit wide; on 64-bit systems, size_t is 64-bit and malloc expects a size_t, not an unsigned int."
