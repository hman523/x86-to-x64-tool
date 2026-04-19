-- | User-facing API for the x86-to-x64 migration tool.
--
-- Typical usage:
--
--   analyzeFile "foo.c"           -- returns the list of issues found
--   lintFile "foo.c"         -- returns (linted source, unresolved issues)
--   analyzeSource "int main(){…}" -- work directly from a string
module X86_to_X64
    ( -- * File-based entry points
      analyzeFile
    , analyzeFileWithCPP
    , lintFile
    , lintFileWithCPP
    , transformFile
    , transformFileWithCPP
      -- * String-based entry points
    , analyzeSource
    , lintSource
    , transformSource
      -- * Re-exports useful to callers
    , Issue(..)
    , IssueTag(..)
    , Severity(..)
    , Category(..)
    , prettyPrintIssues
    ) where

import Language.C.Pretty     (pretty)
import Text.PrettyPrint      (render)

import Parser.Parser              (parseSourceFile, parseSourceFileWithCPP, parseSourceString)
import Analysis.Analysis          (analysis)
import Analysis.IssueTypes        (Issue(..), IssueTag(..), Severity(..), Category(..),
                                   prettyPrintIssues)
import Linter.Linter              (lint)
import Transformer.Transformer    (transform, addRequiredIncludes)

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

-- | Like 'analyzeFile', but runs the C preprocessor (GCC) first.
--   This is needed for source files that use @#include@ directives or macros.
analyzeFileWithCPP :: FilePath -> IO (Either String [Issue])
analyzeFileWithCPP path = do
    result <- parseSourceFileWithCPP path
    return $ case result of
        Left err  -> Left (show err)
        Right ast -> Right (analysis ast)

-- | Parse, analyse, and lint a C source file.
--   Returns @Left errMsg@ on a parse failure, otherwise
--   @Right (lintedSource, unresolvedIssues)@.
lintFile :: FilePath -> IO (Either String (String, [Issue]))
lintFile path = do
    result <- parseSourceFile path
    return $ case result of
        Left err  -> Left (show err)
        Right ast ->
            let issues              = analysis ast
                (ast', unresolved)  = lint ast issues
            in Right (addRequiredIncludes (render (pretty ast')), unresolved)

-- | Like 'lintFile', but runs the C preprocessor (GCC) first.
lintFileWithCPP :: FilePath -> IO (Either String (String, [Issue]))
lintFileWithCPP path = do
    result <- parseSourceFileWithCPP path
    return $ case result of
        Left err  -> Left (show err)
        Right ast ->
            let issues              = analysis ast
                (ast', unresolved)  = lint ast issues
            in Right (addRequiredIncludes (render (pretty ast')), unresolved)

-- ---------------------------------------------------------------------------
-- String-based entry points
-- ---------------------------------------------------------------------------

-- | Analyse C source code supplied as a 'String'.
--   Returns @Left errMsg@ on a parse failure, otherwise @Right issues@.
analyzeSource :: String -> Either String [Issue]
analyzeSource src = case parseSourceString src of
    Left err  -> Left (show err)
    Right ast -> Right (analysis ast)

-- | Analyse and lint C source code supplied as a 'String'.
--   Returns @Left errMsg@ on a parse failure, otherwise
--   @Right (lintedSource, unresolvedIssues)@.
lintSource :: String -> Either String (String, [Issue])
lintSource src = case parseSourceString src of
    Left err  -> Left (show err)
    Right ast ->
        let issues             = analysis ast
            (ast', unresolved) = lint ast issues
        in Right (addRequiredIncludes (render (pretty ast')), unresolved)

-- ---------------------------------------------------------------------------
-- Transformer entry points
-- ---------------------------------------------------------------------------

-- | Parse and transform a C source file, rewriting @long@ / @unsigned long@
--   declarations to semantically equivalent fixed-width types.
--   Returns @Left errMsg@ on a parse failure, otherwise @Right transformedSource@.
transformFile :: FilePath -> IO (Either String String)
transformFile path = do
    result <- parseSourceFile path
    return $ case result of
        Left err  -> Left (show err)
        Right ast -> Right (addRequiredIncludes (render (pretty (transform ast))))

-- | Like 'transformFile', but runs the C preprocessor (GCC) first.
--   This is needed for source files that use @#include@ directives.
transformFileWithCPP :: FilePath -> IO (Either String String)
transformFileWithCPP path = do
    result <- parseSourceFileWithCPP path
    return $ case result of
        Left err  -> Left (show err)
        Right ast -> Right (addRequiredIncludes (render (pretty (transform ast))))

-- | Transform C source code supplied as a 'String'.
--   Returns @Left errMsg@ on a parse failure, otherwise @Right transformedSource@.
transformSource :: String -> Either String String
transformSource src = case parseSourceString src of
    Left err  -> Left (show err)
    Right ast -> Right (addRequiredIncludes (render (pretty (transform ast))))
