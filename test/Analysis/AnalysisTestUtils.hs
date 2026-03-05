module Analysis.AnalysisTestUtils where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import Language.C.Syntax.AST
import Parser.Parser (parseSource)
import Analysis.UtilTypes

shouldFlagError :: String -> String -> (CTranslUnit -> [Issue]) -> Spec
shouldFlagError testName code checker =
  it testName $ do
    case parseSource (BS.pack code) of
      Left err -> fail $ show err
      Right ast -> do
        let issues = checker ast
        length issues `shouldSatisfy` (> 0)

shouldNotFlagError :: String -> String -> (CTranslUnit -> [Issue]) -> Spec
shouldNotFlagError testName code checker =
  it testName $ do
    case parseSource (BS.pack code) of
      Left err -> fail $ show err
      Right ast -> do
        let issues = checker ast
        length issues `shouldBe` 0

shouldFlagErrorWithDetails :: String -> String -> (CTranslUnit -> [Issue]) -> IssueTag -> Maybe String -> Spec
shouldFlagErrorWithDetails testName code checker expectedTag expectedCodeSnippet =
  it testName $ do
    case parseSource (BS.pack code) of
      Left err -> fail $ show err
      Right ast -> do
        let issues = checker ast
        length issues `shouldSatisfy` (> 0)
        let issue = head issues
        issueType issue `shouldBe` expectedTag
        case expectedCodeSnippet of
          Just snippet -> code `shouldContainSnippet` snippet
          Nothing -> return ()

shouldContainSnippet :: String -> String -> Expectation
shouldContainSnippet sourceCode snippet =
  sourceCode `shouldContain` snippet