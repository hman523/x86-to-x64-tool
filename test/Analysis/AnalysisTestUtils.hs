module Analysis.AnalysisTestUtils where

import Test.Hspec
import Data.List (sort, nub, (\\))
import Language.C.Syntax.AST
import Parser.Parser (parseSourceString)
import Analysis.IssueTypes

shouldFlagError :: String -> String -> (CTranslUnit -> [Issue]) -> Spec
shouldFlagError testName code checker =
  it testName $ do
    case parseSourceString code of
      Left err -> fail $ show err
      Right ast -> do
        let issues = checker ast
        length issues `shouldSatisfy` (> 0)

shouldNotFlagError :: String -> String -> (CTranslUnit -> [Issue]) -> Spec
shouldNotFlagError testName code checker =
  it testName $ do
    case parseSourceString code of
      Left err -> fail $ show err
      Right ast -> do
        let issues = checker ast
        length issues `shouldBe` 0

shouldFlagErrorWithDetails :: String -> String -> (CTranslUnit -> [Issue]) -> IssueTag -> Maybe String -> Spec
shouldFlagErrorWithDetails testName code checker expectedTag expectedCodeSnippet =
  it testName $ do
    case parseSourceString code of
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

-- | Assert that the checker finds exactly @n@ issues.
shouldFlagNIssues :: String -> String -> (CTranslUnit -> [Issue]) -> Int -> Spec
shouldFlagNIssues testName code checker n =
  it testName $ do
    case parseSourceString code of
      Left err  -> fail $ show err
      Right ast -> length (checker ast) `shouldBe` n

-- | Assert that the checker finds at least @n@ issues.
shouldFlagAtLeastNIssues :: String -> String -> (CTranslUnit -> [Issue]) -> Int -> Spec
shouldFlagAtLeastNIssues testName code checker n =
  it testName $ do
    case parseSourceString code of
      Left err  -> fail $ show err
      Right ast -> length (checker ast) `shouldSatisfy` (>= n)

-- | Assert that every tag in @expectedTags@ appears at least once in the found issues.
shouldFlagAllTags :: String -> String -> (CTranslUnit -> [Issue]) -> [IssueTag] -> Spec
shouldFlagAllTags testName code checker expectedTags =
  it testName $ do
    case parseSourceString code of
      Left err  -> fail $ show err
      Right ast -> do
        let foundTags = map issueType (checker ast)
        let missing   = nub expectedTags \\ nub foundTags
        missing `shouldBe` []

-- | Assert that the checker finds issues whose tags are exactly @expectedTags@
--   (order-insensitive, duplicates matter).
shouldFlagExactTags :: String -> String -> (CTranslUnit -> [Issue]) -> [IssueTag] -> Spec
shouldFlagExactTags testName code checker expectedTags =
  it testName $ do
    case parseSourceString code of
      Left err  -> fail $ show err
      Right ast -> do
        let foundTags = map issueType (checker ast)
        sort (map show foundTags) `shouldBe` sort (map show expectedTags)

-- | Helper to parse a C snippet, failing hard on parse error
parseSrc :: String -> CTranslUnit
parseSrc src = case parseSourceString src of
    Left err  -> error $ "Parse error: " ++ show err
    Right ast -> ast