-- | User-facing API for the x86-to-x64 migration tool.
--
-- Typical usage:
--
--   analyzeFile "foo.c"           -- returns the list of issues found
--   transformFile "foo.c"         -- returns (transformed source, unresolved issues)
--   analyzeSource "int main(){…}" -- work directly from a string
module X86_to_X64
    ( -- * File-based entry points
      analyzeFile
    , transformFile
      -- * String-based entry points
    , analyzeSource
    , transformSource
      -- * Re-exports useful to callers
    , Issue(..)
    , IssueTag(..)
    , Severity(..)
    , prettyPrintIssues
    ) where

import Language.C.Pretty     (pretty)
import Text.PrettyPrint      (render)

import Parser.Parser         (parseSourceFile, parseSourceString)
import Analysis.Analysis     (analysis)
import Analysis.UtilTypes    (Issue(..), IssueTag(..), Severity(..), prettyPrintIssues)
import Transformation.Transformation (transformation)

-- ---------------------------------------------------------------------------
-- File-based entry points
-- ---------------------------------------------------------------------------

-- | Parse and analyse a C source file.
--   Returns @Left errMsg@ on a parse failure, otherwise @Right issues@.
analyzeFile :: FilePath -> IO (Either String [Issue])
analyzeFile path = do
    result <- parseSourceFile path
    return $ case result of
        Left err  -> Left (show err)
        Right ast -> Right (analysis ast)

-- | Parse, analyse, and transform a C source file.
--   Returns @Left errMsg@ on a parse failure, otherwise
--   @Right (transformedSource, unresolvedIssues)@.
transformFile :: FilePath -> IO (Either String (String, [Issue]))
transformFile path = do
    result <- parseSourceFile path
    return $ case result of
        Left err  -> Left (show err)
        Right ast ->
            let issues              = analysis ast
                (ast', unresolved)  = transformation ast issues
            in Right (render (pretty ast'), unresolved)

-- ---------------------------------------------------------------------------
-- String-based entry points
-- ---------------------------------------------------------------------------

-- | Analyse C source code supplied as a 'String'.
--   Returns @Left errMsg@ on a parse failure, otherwise @Right issues@.
analyzeSource :: String -> Either String [Issue]
analyzeSource src = case parseSourceString src of
    Left err  -> Left (show err)
    Right ast -> Right (analysis ast)

-- | Analyse and transform C source code supplied as a 'String'.
--   Returns @Left errMsg@ on a parse failure, otherwise
--   @Right (transformedSource, unresolvedIssues)@.
transformSource :: String -> Either String (String, [Issue])
transformSource src = case parseSourceString src of
    Left err  -> Left (show err)
    Right ast ->
        let issues             = analysis ast
            (ast', unresolved) = transformation ast issues
        in Right (render (pretty ast'), unresolved)
