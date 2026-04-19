module Linter.IntegrationTests where

import Test.Hspec
import X86_to_X64

linterIntegrationSpec :: Spec
linterIntegrationSpec = describe "Linter Integration" $ do

  -- -------------------------------------------------------------------
  -- analyzeFile
  -- -------------------------------------------------------------------
  describe "analyzeFile" $ do

    it "returns Left for a file that fails to parse" $ do
      result <- analyzeFile "test/c_progs/invalid.c"
      result `shouldSatisfy` isLeft

    it "returns Right [] for a clean source file" $ do
      result <- analyzeFile "test/c_progs/hello.c"
      result `shouldSatisfy` isRightEmpty

    it "returns Right (non-empty) for a file with portability issues" $ do
      result <- analyzeFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` isRightNonEmpty

    it "flags CastPointerToInt in ptr_issues.c" $ do
      result <- analyzeFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` hasTag CastPointerToInt

    it "flags PtrDiffStoredAs32bit in ptr_issues.c" $ do
      result <- analyzeFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` hasTag PtrDiffStoredAs32bit

    it "flags FnsReturnPtrAsInt in ptr_issues.c" $ do
      result <- analyzeFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` hasTag FnsReturnPtrAsInt

    it "flags StructsWithMixedPtrNonPtrMembers in ptr_issues.c" $ do
      result <- analyzeFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` hasTag StructsWithMixedPtrNonPtrMembers

  -- -------------------------------------------------------------------
  -- lintFile
  -- -------------------------------------------------------------------
  describe "lintFile" $ do

    it "returns Left for a file that fails to parse" $ do
      result <- lintFile "test/c_progs/invalid.c"
      result `shouldSatisfy` isLeft

    it "returns Right for a file with portability issues" $ do
      result <- lintFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` isRightPair

    it "linted source contains intptr_t after cast rewrite" $ do
      result <- lintFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "linted source contains ptrdiff_t after ptr-diff variable rewrite" $ do
      result <- lintFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "linted source contains size_t after sizeof-to-int rewrite" $ do
      result <- lintFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "size_t"

    it "leaves StructsWithMixedPtrNonPtrMembers unresolved" $ do
      result <- lintFile "test/c_progs/ptr_issues.c"
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (StructsWithMixedPtrNonPtrMembers `elem`)

  -- -------------------------------------------------------------------
  -- analyzeSource
  -- -------------------------------------------------------------------
  describe "analyzeSource" $ do

    it "returns Left on unparseable input" $
      analyzeSource "int;" `shouldSatisfy` isLeft

    it "returns Right [] for issue-free code" $
      analyzeSource "int f(void) { return 0; }" `shouldSatisfy` isRightEmpty

    it "returns Right (non-empty) for code with a pointer cast" $
      analyzeSource "int f(void) { int *p = 0; int x = (int)p; return x; }"
        `shouldSatisfy` isRightNonEmpty

  -- -------------------------------------------------------------------
  -- lintSource
  -- -------------------------------------------------------------------
  describe "lintSource" $ do

    it "returns Left on unparseable input" $
      lintSource "int;" `shouldSatisfy` isLeft

    it "rewrites (int)ptr cast to (intptr_t)ptr" $
      case lintSource "int f(void) { int *p = 0; int x = (int)p; return x; }" of
        Left err       -> expectationFailure err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "rewrites int variable holding ptrdiff to ptrdiff_t" $
      case lintSource "void f(void) { int *p = 0; int *q = 0; int d; d = p - q; }" of
        Left err       -> expectationFailure err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "returns unresolvable issues in the unresolved list" $
      case lintSource "struct S { int *p; int x; }; void f(void) { }" of
        Left err              -> expectationFailure err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (StructsWithMixedPtrNonPtrMembers `elem`)

  -- -------------------------------------------------------------------
  -- everything at once
  -- -------------------------------------------------------------------
  describe "everything at once" $ do

    result <- runIO $ lintFile "test/c_progs/everything_at_once.c"

    it "lints everything_at_once.c without error" $
      result `shouldSatisfy` isRightPair

    -- TypeSize: cast rewrites -----------------------------------------

    it "rewrites (int)ptr cast to (intptr_t) — CastPointerToInt" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "rewrites (T*)long cast to (intptr_t) — CastLongToPointer" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "retypes unsigned int malloc-size variable to size_t — UsingUIntAsMemSize" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "size_t"

    -- FunctionSignatures rewrites -------------------------------------

    it "wraps returned pointer in (intptr_t) cast — FnsReturnPtrAsInt" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "retypes int parameter receiving a pointer to intptr_t — FnsParamDeclaredAsIntTakesPtr" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    -- PointerMath rewrites --------------------------------------------

    it "retypes ptr-diff variable to ptrdiff_t — PtrDiffStoredAs32bit" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "retypes array-index variable to ptrdiff_t — ArrayIndexingIntInArrayOver2tothe31size" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    -- Comparison rewrites ---------------------------------------------

    it "retypes int loop counter over pointer range to ptrdiff_t — LoopCounterAsIntWhenIteratingOverPtrArrays" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "retypes int file-offset variable to off_t — UsingIntForFileOffsets" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "off_t"

    -- MemoryAllocation rewrites ---------------------------------------

    it "retypes int malloc-size variables to size_t — UsingIntToStoreAllocationSizes" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "size_t"

    -- Alignment rewrites ----------------------------------------------

    it "retypes int variable storing sizeof result to size_t — SizeofStoredIn32Bits" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "size_t"

    -- FormatStrings rewrites ------------------------------------------

    it "rewrites %d to %p for pointer argument — DUsedWithPtr" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "%p"

    it "rewrites %ld to %td for long argument — LdUsedWithLongAssuming64bits" $
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "%td"

    -- Unresolvable issues ---------------------------------------------

    it "StructsWithMixedPtrNonPtrMembers is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (StructsWithMixedPtrNonPtrMembers `elem`)

    it "UnionsContainingPtrAndInts is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (UnionsContainingPtrAndInts `elem`)

    it "AsmBlocks is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (AsmBlocks `elem`)

    it "InlineAsmWithx86Instructions is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (InlineAsmWithx86Instructions `elem`)

    it "X86SpecificCompilerIntrinsics is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (X86SpecificCompilerIntrinsics `elem`)

    it "PackingPtrsWithFlagsInInt is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (PackingPtrsWithFlagsInInt `elem`)

    it "BitShiftsOnPtr is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (BitShiftsOnPtr `elem`)

    it "AllocationSizeCalcsMayOverflow is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (AllocationSizeCalcsMayOverflow `elem`)

    it "WritingPtrDirectToFile is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (WritingPtrDirectToFile `elem`)

    it "SendingPtrsOverNetwork is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (SendingPtrsOverNetwork `elem`)

    it "PtrInSharedMemory is left unresolved" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (PtrInSharedMemory `elem`)

    it "at least 20 issues remain unresolved after linting" $
      case result of
        Left err              -> fail err
        Right (_, unresolved) -> length unresolved `shouldSatisfy` (>= 20)

-- ---------------------------------------------------------------------------
-- Predicate helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRightEmpty :: Either a [b] -> Bool
isRightEmpty (Right []) = True
isRightEmpty _          = False

isRightNonEmpty :: Either a [b] -> Bool
isRightNonEmpty (Right (_:_)) = True
isRightNonEmpty _             = False

isRightPair :: Either a (b, c) -> Bool
isRightPair (Right _) = True
isRightPair _         = False

hasTag :: IssueTag -> Either a [Issue] -> Bool
hasTag tag (Right issues) = tag `elem` map issueType issues
hasTag _   (Left _)       = False
