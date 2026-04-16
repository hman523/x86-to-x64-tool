module Transformer.IntegrationTests where

import Test.Hspec
import X86_to_X64

transformerIntegrationSpec :: Spec
transformerIntegrationSpec = describe "Transformer Integration" $ do

    -- -------------------------------------------------------------------
    -- transformFile
    -- -------------------------------------------------------------------
    describe "transformFile" $ do

        it "returns Left for a file that fails to parse" $ do
            result <- transformFile "test/c_progs/invalid.c"
            result `shouldSatisfy` isLeft

        it "returns Right for a clean source file" $ do
            result <- transformFile "test/c_progs/hello.c"
            result `shouldSatisfy` isRight

        -- long_classification.c: all five abstract type categories
        it "produces int32_t in long_classification.c (number case)" $ do
            result <- transformFile "test/c_progs/long_classification.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "int32_t"

        it "produces intptr_t in long_classification.c (pointer case)" $ do
            result <- transformFile "test/c_progs/long_classification.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "intptr_t"

        it "produces size_t in long_classification.c (size case)" $ do
            result <- transformFile "test/c_progs/long_classification.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "size_t"

        it "produces ptrdiff_t in long_classification.c (offset case)" $ do
            result <- transformFile "test/c_progs/long_classification.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "ptrdiff_t"

        it "produces uint32_t in long_classification.c (bit-sequence case)" $ do
            result <- transformFile "test/c_progs/long_classification.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "uint32_t"

        -- ptr_issues.c: SE variable retypings applied via transformer
        it "rewrites ptr-diff int variable to ptrdiff_t in ptr_issues.c" $ do
            result <- transformFile "test/c_progs/ptr_issues.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "ptrdiff_t"

        it "rewrites sizeof-to-int variable to size_t in ptr_issues.c" $ do
            result <- transformFile "test/c_progs/ptr_issues.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "size_t"

        it "does NOT rewrite (int)ptr cast in ptr_issues.c (not SE: receiving var stays int)" $ do
            result <- transformFile "test/c_progs/ptr_issues.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldNotContain` "intptr_t"

    -- -------------------------------------------------------------------
    -- transformSource
    -- -------------------------------------------------------------------
    describe "transformSource" $ do

        it "returns Left on unparseable input" $
            transformSource "int;" `shouldSatisfy` isLeft

        it "returns Right for valid source" $
            transformSource "int f(void) { return 0; }" `shouldSatisfy` isRight

        it "transforms long local to int32_t" $
            case transformSource "void f(void) { long n = 42; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "int32_t"

        it "transforms long (pointer) to intptr_t" $
            case transformSource "void f(void *p) { long x = (long)p; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "intptr_t"

        it "transforms long (size) to size_t" $
            case transformSource "void f(void) { long sz = sizeof(int); }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "size_t"

        it "transforms long (offset) to ptrdiff_t" $
            case transformSource "void f(int *a, int *b) { long d = a - b; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "ptrdiff_t"

        it "transforms long (bit-sequence) to uint32_t" $
            case transformSource "void f(long val) { long m = val & 255; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "uint32_t"

        it "does not touch non-long types" $
            case transformSource "void f(void) { int n = 0; }" of
                Left err  -> fail err
                Right src -> src `shouldNotContain` "int32_t"

        it "rewrites int ptr-diff variable to ptrdiff_t" $
            case transformSource "void f(void) { int *p = 0; int *q = 0; int d; d = p - q; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "ptrdiff_t"

        it "rewrites int sizeof variable to size_t" $
            case transformSource "void f(void) { int n; n = sizeof(int); }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "size_t"

        it "rewrites int parameter receiving pointer to intptr_t" $
            case transformSource "void f(int h) { int *r = 0; h = r; }" of
                Left err  -> fail err
                Right src -> src `shouldContain` "intptr_t"

        it "does NOT rewrite (int)ptr cast (not SE: no matching variable retype)" $
            case transformSource "void f(void) { int *p = 0; int x = (int)p; }" of
                Left err  -> fail err
                Right src -> src `shouldNotContain` "intptr_t"

        -- struct_long.c: struct members, typedefs, sizeof, return type, cast sync
        it "rewrites long struct member to int32_t in struct_long.c" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "int32_t"

        it "rewrites unsigned long struct member to uint32_t in struct_long.c" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "uint32_t"

        it "rewrites typedef long word_t in struct_long.c" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "int32_t"

        it "rewrites long return type in struct_long.c (number case)" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "int32_t"

        it "rewrites long return type to intptr_t in struct_long.c (pointer case)" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "intptr_t"

        it "rewrites sizeof(long) in struct_long.c" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "sizeof(int32_t)"

        it "syncs (long) cast to (intptr_t) in struct_long.c" $ do
            result <- transformFile "test/c_progs/struct_long.c"
            case result of
                Left err  -> fail err
                Right src -> src `shouldContain` "(intptr_t)"

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
