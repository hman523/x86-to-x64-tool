module Parser.ParserTests where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Parser.Parser (parseSource)

assertParses :: String -> String -> Assertion
assertParses msg src =
  case parseSource (BS.pack src) of
    Left err -> assertFailure $ msg ++ ": " ++ show err
    Right _  -> return ()

assertFails :: String -> String -> Assertion
assertFails msg src =
  case parseSource (BS.pack src) of
    Left _  -> return ()
    Right _ -> assertFailure $ msg ++ ": expected failure but succeeded"

parserSpec :: Spec
parserSpec =
  describe "Parser" $ do
    it "parses a simple function" $
      assertParses "simple function" "int main() { return 0; }"

    it "parses a variable declaration" $
      assertParses "variable declaration" "int x = 42;"

    it "rejects invalid C" $
      assertFails "invalid C" "@@@not valid C@@@"