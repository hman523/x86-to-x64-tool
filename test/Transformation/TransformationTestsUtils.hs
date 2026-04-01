module Transformation.TransformationTestsUtils where

import Test.Hspec
import Data.List (sort)
import Data.Maybe (isNothing)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Text.PrettyPrint (render)
import Parser.Parser (parseSourceString)
import Analysis.UtilTypes

-- | Parse source, run the analyser to obtain issues, run the module-level
-- transformer, assert no issues remain unresolved, and re-run the analyser
-- on the transformed AST to verify the issues are gone at the semantic level.
shouldFullyTransform
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> Spec
shouldFullyTransform name code analyser transformer =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        length issues `shouldSatisfy` (> 0)
        let (ast', unresolved) = transformer ast issues
        unresolved `shouldSatisfy` null
        analyser ast' `shouldSatisfy` null

-- | Like 'shouldFullyTransform' but calls a per-issue transformer on the
-- first flagged issue rather than the module-level dispatcher.
shouldResolveIssue
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue))
  -> Spec
shouldResolveIssue name code analyser transformer =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        length issues `shouldSatisfy` (> 0)
        let (ast', mi) = transformer ast (head issues)
        mi `shouldSatisfy` isNothing
        analyser ast' `shouldSatisfy` null

-- | Parse source, run the analyser, run the transformer, and assert the
-- pretty-printed output contains the expected substring.
shouldTransformTo
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> String                                             -- ^ expected substring
  -> Spec
shouldTransformTo name code analyser transformer expected =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        let (ast', _) = transformer ast issues
        render (pretty ast') `shouldContain` expected

-- | Assert that the transformer leaves exactly the listed tags unresolved
-- (order-insensitive).
shouldLeaveUnresolved
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> [IssueTag]
  -> Spec
shouldLeaveUnresolved name code analyser transformer expectedTags =
  it name $ do
    case parseSourceString code of
      Left err  -> fail (show err)
      Right ast -> do
        let issues = analyser ast
        let (_, unresolved) = transformer ast issues
        sort (map (show . issueType) unresolved)
          `shouldBe` sort (map show expectedTags)

