module Transformation.TransformationTestsUtils where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import Language.C.Syntax.AST
import Parser.Parser (parseSourceString)
import Analysis.UtilTypes

-- | Parse source, run the analyser to obtain issues, run the transformer,
--   and assert the output C source contains the expected string.
shouldTransformTo
  :: String                                             -- ^ test name
  -> String                                             -- ^ C source input
  -> (CTranslUnit -> [Issue])                           -- ^ analyser
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])) -- ^ transformer
  -> String                                             -- ^ expected substring in output
  -> Spec
shouldTransformTo = undefined

-- | Assert that the transformer resolves every issue (no unresolved issues remain).
shouldFullyTransform
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> Spec
shouldFullyTransform = undefined

-- | Assert that the transformer leaves exactly the given tags unresolved.
shouldLeaveUnresolved
  :: String
  -> String
  -> (CTranslUnit -> [Issue])
  -> (CTranslUnit -> [Issue] -> (CTranslUnit, [Issue]))
  -> [IssueTag]
  -> Spec
shouldLeaveUnresolved = undefined

