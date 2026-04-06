module Transformation.IntegrationTests where

import Test.Hspec
import X86_to_X64
import Analysis.IssueTypes

transformationIntegrationSpec :: Spec
transformationIntegrationSpec = describe "Transformation Integration" $ do

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
  -- transformFile
  -- -------------------------------------------------------------------
  describe "transformFile" $ do

    it "returns Left for a file that fails to parse" $ do
      result <- transformFile "test/c_progs/invalid.c"
      result `shouldSatisfy` isLeft

    it "returns Right for a file with portability issues" $ do
      result <- transformFile "test/c_progs/ptr_issues.c"
      result `shouldSatisfy` isRightPair

    it "transformed source contains intptr_t after cast rewrite" $ do
      result <- transformFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "transformed source contains ptrdiff_t after ptr-diff variable rewrite" $ do
      result <- transformFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "transformed source contains size_t after sizeof-to-int rewrite" $ do
      result <- transformFile "test/c_progs/ptr_issues.c"
      case result of
        Left err       -> fail err
        Right (src, _) -> src `shouldContain` "size_t"

    it "leaves StructsWithMixedPtrNonPtrMembers unresolved" $ do
      result <- transformFile "test/c_progs/ptr_issues.c"
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
  -- transformSource
  -- -------------------------------------------------------------------
  describe "transformSource" $ do

    it "returns Left on unparseable input" $
      transformSource "int;" `shouldSatisfy` isLeft

    it "rewrites (int)ptr cast to (intptr_t)ptr" $
      case transformSource "int f(void) { int *p = 0; int x = (int)p; return x; }" of
        Left err       -> expectationFailure err
        Right (src, _) -> src `shouldContain` "intptr_t"

    it "rewrites int variable holding ptrdiff to ptrdiff_t" $
      case transformSource "void f(void) { int *p = 0; int *q = 0; int d; d = p - q; }" of
        Left err       -> expectationFailure err
        Right (src, _) -> src `shouldContain` "ptrdiff_t"

    it "returns unresolvable issues in the unresolved list" $
      case transformSource "struct S { int *p; int x; }; void f(void) { }" of
        Left err              -> expectationFailure err
        Right (_, unresolved) ->
          map issueType unresolved `shouldSatisfy` (StructsWithMixedPtrNonPtrMembers `elem`)

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
