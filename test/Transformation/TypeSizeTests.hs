module Transformation.TypeSizeTests where

import Test.Hspec
import Language.C.Pretty (pretty)
import Text.PrettyPrint (render)
import Parser.Parser (parseSourceString)
import Transformation.TransformationTestsUtils
import Transformation.TypeSize
import Analysis.TypeSize (analyzeTypeSizeIssues)
import Analysis.IssueTypes

typeSizeTransformSpec :: Spec
typeSizeTransformSpec = describe "TypeSize Transformations" $ do
  testTransformCastPointerToInt
  testTransformCastPointerToUInt
  testTransformCastIntToPointer
  testTransformCastLongToPointer
  testTransformSizeOfIntIsVoid
  testTransformSizeOfLongIsVoid
  testTransformUsingIntAsSizet
  testTransformUsingIntAsPtrdifft
  testTransformUsingUIntAsMemSize
  testTransformMultiple

testTransformCastPointerToInt :: Spec
testTransformCastPointerToInt =
  describe "transformCastPointerToInt" $ do
    shouldTransformTo "rewrites (int)ptr to (intptr_t)ptr"
      "int main() { int *ptr = 0; int x = (int)ptr; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "intptr_t"
    shouldLeaveUnresolved "leaves unresolvable sizeof issue alone when transforming cast"
      "int main() { int *ptr = 0; int x = (int)ptr; if (sizeof(int) == sizeof(void*)) return 1; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      [SizeOfIntIsVoid]

testTransformCastPointerToUInt :: Spec
testTransformCastPointerToUInt =
  describe "transformCastPointerToUInt" $ do
    shouldTransformTo "rewrites (unsigned int)ptr to (uintptr_t)ptr"
      "int main() { int *ptr = 0; unsigned int x = (unsigned int)ptr; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "uintptr_t"

testTransformCastIntToPointer :: Spec
testTransformCastIntToPointer =
  describe "transformCastIntToPointer" $ do
    shouldTransformTo "rewrites (int*)x to (intptr_t)x"
      "int main() { int x = 5; void *p = (int*)x; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "intptr_t"

testTransformCastLongToPointer :: Spec
testTransformCastLongToPointer =
  describe "transformCastLongToPointer" $ do
    shouldTransformTo "rewrites (int*)long to (intptr_t)long"
      "int main() { long x = 5; void *p = (int*)x; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "intptr_t"

testTransformSizeOfIntIsVoid :: Spec
testTransformSizeOfIntIsVoid =
  describe "transformSizeOfIntIsVoid" $ do
    shouldLeaveUnresolved "leaves sizeof(int)==sizeof(void*) unresolved"
      "int f() { return sizeof(int) == sizeof(void*); }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      [SizeOfIntIsVoid]

testTransformSizeOfLongIsVoid :: Spec
testTransformSizeOfLongIsVoid =
  describe "transformSizeOfLongIsVoid" $ do
    shouldLeaveUnresolved "leaves sizeof(long)==sizeof(void*) unresolved"
      "int f() { return sizeof(long) == sizeof(void*); }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      [SizeOfLongIsVoid]

testTransformUsingIntAsSizet :: Spec
testTransformUsingIntAsSizet =
  describe "transformUsingIntAsSizet" $ do
    shouldTransformTo "rewrites size variable declaration to size_t"
      "int main() { int x = 5; unsigned long size; size = x; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "size_t"

testTransformUsingIntAsPtrdifft :: Spec
testTransformUsingIntAsPtrdifft =
  describe "transformUsingIntAsPtrdifft" $ do
    shouldTransformTo "rewrites diff variable declaration to ptrdiff_t"
      "int main() { int x = 5; long diff; diff = x; return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "ptrdiff_t"

testTransformUsingUIntAsMemSize :: Spec
testTransformUsingUIntAsMemSize =
  describe "transformUsingUIntAsMemSize" $ do
    shouldTransformTo "rewrites unsigned int variable passed to malloc to size_t"
      "int main() { unsigned int size = 10; void *ptr = malloc(size); return 0; }"
      analyzeTypeSizeIssues
      transformTypeSizeIssues
      "size_t"

-- | A single C function that triggers all four resolvable TypeSize issue
-- kinds plus one unresolvable sizeof comparison.  The transformer should
-- rewrite every resolvable cast/declaration and leave only SizeOfIntIsVoid.
testTransformMultiple :: Spec
testTransformMultiple =
  describe "multiple TypeSize transformations in one snippet" $ do
    it "rewrites all resolvable issues and leaves sizeof comparison unresolved" $ do
      let code = unlines
            [ "int main() {"
            , "    int *ptr = 0;"
            , "    int a = (int)ptr;"               -- CastPointerToInt  -> intptr_t
            , "    unsigned int b = (unsigned int)ptr;" -- CastPointerToUInt -> uintptr_t
            , "    int n = 5;"
            , "    unsigned long size;"
            , "    size = n;"                        -- UsingIntAsSizet   -> size_t
            , "    long diff;"
            , "    diff = n;"                        -- UsingIntAsPtrdifft -> ptrdiff_t
            , "    if (sizeof(int) == sizeof(void*)) return 1;" -- SizeOfIntIsVoid (unresolvable)
            , "    return 0;"
            , "}"
            ]
      case parseSourceString code of
        Left err  -> fail (show err)
        Right ast -> do
          let issues = analyzeTypeSizeIssues ast
          length issues `shouldBe` 5
          let (ast', unresolved) = transformTypeSizeIssues ast issues
          let output = render (pretty ast')
          -- All four resolvable rewrites should appear in the output
          output `shouldContain` "intptr_t"
          output `shouldContain` "uintptr_t"
          output `shouldContain` "size_t"
          output `shouldContain` "ptrdiff_t"
          -- Only the sizeof comparison should remain unresolved
          length unresolved `shouldBe` 1
          issueType (head unresolved) `shouldBe` SizeOfIntIsVoid
