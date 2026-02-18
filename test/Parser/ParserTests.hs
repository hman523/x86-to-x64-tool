module Parser.ParserTests where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Parser.Parser (parseSource)
import Language.C
import Data.Generics (everywhere, mkT)


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

testHelloAST :: String -> Assertion
testHelloAST source = 
  case parseSource (BS.pack source) of
    Left err -> assertFailure $ show err
    Right ast -> assertEqual "AST mismatch" (show $ stripPos expectedAST) (show $ stripPos ast)
  where
    stripPos = everywhere (mkT $ \_ -> undefNode :: NodeInfo)
    
    expectedAST = CTranslUnit 
      [CFDefExt (CFunDef
        [CTypeSpec (CIntType undefNode)]
        (CDeclr (Just (internalIdent "main")) 
                [CFunDeclr (Right ([], False)) [] undefNode] 
                Nothing [] undefNode)
        []
        (CCompound []
          [ CBlockStmt (CExpr (Just (CCall 
              (CVar (internalIdent "printf") undefNode)
              [CConst (CStrConst (cString "Hello World") undefNode)]
              undefNode)) undefNode)
          , CBlockStmt (CReturn (Just (CConst (CIntConst (cInteger 0) undefNode))) undefNode)
          ]
          undefNode)
        undefNode)]
      undefNode


parserSpec :: Spec
parserSpec = do
  
  helloc <- runIO $ readFile "test/c_progs/hello.c"
  invalidc <- runIO $ readFile "test/c_progs/invalid.c"
  
  describe "Parser" $ do
    it "parses a simple function" $
      assertParses "simple function" "int main() { return 0; }"

    it "parses a variable declaration" $
      assertParses "variable declaration" "int x = 42;"

    it "rejects invalid C" $
      assertFails "invalid C" "@@@not valid C@@@"

    it "parses valid file" $ 
      assertParses "valid file" helloc
    
    it "rejects invalid file" $
      assertFails "invalid file" invalidc

    it "produces proper AST for hello.c" $
      testHelloAST helloc