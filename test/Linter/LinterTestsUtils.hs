module Linter.LinterTestsUtils where

import Test.Hspec
import Data.List (sort)
import Data.Maybe (isNothing)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Text.PrettyPrint (render)
import Parser.Parser (parseSourceString)
import Analysis.IssueTypes

-- | Parse source, run the analyser to obtain issues, run the module-level
-- linter, assert no issues remain unresolved, and re-run the analyser
-- on the linted AST to verify the issues are gone at the semantic level.
shouldFullyLint
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> Spec
shouldFullyLint name code analyser linter =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        length issues `shouldSatisfy` (> 0)
        let (ast', unresolved) = linter ast issues
        unresolved `shouldSatisfy` null
        analyser ast' `shouldSatisfy` null

-- | Like 'shouldFullyLint' but calls a per-issue linter on the
-- first flagged issue rather than the module-level dispatcher.
shouldResolveIssue
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue))
  -> Spec
shouldResolveIssue name code analyser linter =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        length issues `shouldSatisfy` (> 0)
        let (ast', mi) = linter ast (head issues)
        mi `shouldSatisfy` isNothing
        analyser ast' `shouldSatisfy` null

-- | Parse source, run the analyser, run the linter, and assert the
-- pretty-printed output contains the expected substring.
shouldLintTo
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> String                                             -- ^ expected substring
  -> Spec
shouldLintTo name code analyser linter expected =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        let (ast', _) = linter ast issues
        render (pretty ast') `shouldContain` expected

-- | Assert that the linted output does NOT contain the given substring.
shouldLintNotTo
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> String                                             -- ^ absent substring
  -> Spec
shouldLintNotTo name code analyser linter absent =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        let (ast', _) = linter ast issues
        render (pretty ast') `shouldNotContain` absent

-- | Assert that the linter leaves exactly the listed tags unresolved
-- (order-insensitive).
shouldLeaveUnresolved
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> [IssueTag]
  -> Spec
shouldLeaveUnresolved name code analyser linter expectedTags =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        let (_, unresolved) = linter ast issues
        sort (map (show . issueType) unresolved)
          `shouldBe` sort (map show expectedTags)

