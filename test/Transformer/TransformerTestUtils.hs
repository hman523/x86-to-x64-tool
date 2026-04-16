module Transformer.TransformerTestUtils where

import Test.Hspec
import Language.C.Pretty (pretty)
import Text.PrettyPrint  (render)

import Parser.Parser           (parseSourceString)
import Transformer.Transformer (transform)

-- | Parse a C source string, run the transformer, and assert the
--   pretty-printed output contains the expected substring.
shouldTransformToContain :: String -> String -> String -> Spec
shouldTransformToContain name src expected =
    it name $ case parseSourceString src of
        Left err  -> fail (show err)
        Right ast -> render (pretty (transform ast)) `shouldContain` expected

-- | Parse a C source string, run the transformer, and assert the
--   pretty-printed output does NOT contain the given substring.
shouldTransformNotToContain :: String -> String -> String -> Spec
shouldTransformNotToContain name src notExpected =
    it name $ case parseSourceString src of
        Left err  -> fail (show err)
        Right ast -> render (pretty (transform ast)) `shouldNotContain` notExpected
