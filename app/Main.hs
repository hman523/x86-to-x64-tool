module Main where

import System.Environment (getArgs, lookupEnv)
import System.Exit        (exitFailure, exitWith, ExitCode(..))
import System.IO          (hPutStrLn, hIsTerminalDevice, stderr, stdout)
import Text.Read          (readMaybe)
import Data.List          (nub)

import X86_to_X64         (analyzeFile, analyzeFileWithCPP,
                           lintFile, lintFileWithCPP,
                           transformFile, transformFileWithCPP,
                           prettyPrintIssues,
                           Issue(..), Severity(..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data Config = Config
    { cfgInputFile  :: FilePath
    , cfgOutputFile :: Maybe FilePath
    , cfgVerbose    :: Bool
    , cfgLint       :: Bool
    , cfgTransform  :: Bool
    , cfgNoColor    :: Bool
    , cfgCPP        :: Bool
    , cfgStrict     :: Bool
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cfgInputFile  = ""
    , cfgOutputFile = Nothing
    , cfgVerbose    = False
    , cfgLint       = False
    , cfgTransform  = False
    , cfgNoColor    = False
    , cfgCPP        = False
    , cfgStrict     = False
    }

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

-- | Walk the argument list, accumulating settings into Config.
-- Returns Left "" to signal --help (exit 0), or Left msg for a real error.
parseArgs :: [String] -> Either String Config
parseArgs args = go args defaultConfig
  where
    go []                cfg
        | null (cfgInputFile cfg) = Left "No input file specified."
        | cfgLint cfg && cfgTransform cfg
            = Left "Cannot use -t and -l together."
        | otherwise               = Right cfg
    go ("-h":_)          _   = Left ""
    go ("--help":_)      _   = Left ""
    go ("-v":rest)         cfg = go rest cfg { cfgVerbose   = True  }
    go ("-l":rest)         cfg = go rest cfg { cfgLint      = True  }
    go ("-t":rest)         cfg = go rest cfg { cfgTransform = True  }
    go ("--transform":rest) cfg = go rest cfg { cfgTransform = True  }
    go ("--no-color":rest) cfg = go rest cfg { cfgNoColor   = True  }
    go ("--cpp":rest)      cfg = go rest cfg { cfgCPP       = True  }
    go ("-cpp":rest)       cfg = go rest cfg { cfgCPP       = True  }
    go ("--strict":rest)   cfg = go rest cfg { cfgStrict    = True  }
    go ("-o":path:rest)  cfg = go rest cfg { cfgOutputFile = Just path }
    go ["-o"]            _   = Left "-o requires an output file path argument."
    go (path:rest)       cfg
        | not (null (cfgInputFile cfg)) = Left ("Unexpected argument: " ++ path)
        | otherwise                     = go rest cfg { cfgInputFile = path }

usageMessage :: String
usageMessage = unlines
    [ "Usage: x86-to-x64-tool <file.c> [options]"
    , ""
    , "Options:"
    , "  -v           Verbose: print a one-sentence explanation for each issue"
    , "  -l           Lint: apply automated x86-to-x64 fixes"
    , "  -t           Transform: rewrite long/unsigned long to fixed-width types"
    , "  -o <file>    Write output to this file (default: <input>.x64.c)"
    , "  --cpp        Run the C preprocessor (gcc) before parsing"
    , "  --strict     Exit with non-zero status on any issue (including warnings)"
    , "  --no-color   Disable colored output"
    , "  -h, --help   Show this help message"
    , ""
    , "Note: -t and -l cannot be used together."
    , "      Analysis is per-file; cross-translation-unit issues are not detected."
    ]

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left ""  -> putStr usageMessage
        Left err -> hPutStrLn stderr ("Error: " ++ err ++ "\n" ++ usageMessage)
                        >> exitFailure
        Right cfg -> run cfg

-- | Query terminal width via the COLUMNS env var (set by zsh/bash),
--   falling back to 80 if unavailable or unparseable.
getTermWidth :: IO Int
getTermWidth = do
    mCols <- lookupEnv "COLUMNS"
    return $ case mCols >>= readMaybe of
        Just n | n > 20 -> n
        _               -> 80

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

run :: Config -> IO ()
run cfg = do
    isTTY     <- hIsTerminalDevice stdout
    termWidth <- getTermWidth
    let useColor = isTTY && not (cfgNoColor cfg)
    if cfgTransform cfg
        then do
            result <- (if cfgCPP cfg then transformFileWithCPP
                                     else transformFile) (cfgInputFile cfg)
            case result of
                Left err  -> hPutStrLn stderr ("Parse error: " ++ err) >> exitFailure
                Right src -> do
                    let outPath = case cfgOutputFile cfg of
                                      Just p  -> p
                                      Nothing -> cfgInputFile cfg ++ ".x64.c"
                    writeFile outPath src
                    putStrLn ("Transformed output written to: " ++ outPath)
        else if cfgLint cfg
        then do
            result <- (if cfgCPP cfg then lintFileWithCPP
                                     else lintFile) (cfgInputFile cfg)
            case result of
                Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitFailure
                Right (src, unresolved) -> do
                    let outPath = case cfgOutputFile cfg of
                                      Just p  -> p
                                      Nothing -> cfgInputFile cfg ++ ".x64.c"
                    writeFile outPath src
                    putStrLn ("Linted output written to: " ++ outPath)
                    if null unresolved
                        then putStrLn "All issues resolved."
                        else do
                            putStrLn (show (length unresolved) ++ " issue(s) could not be automatically resolved:")
                            putStr (prettyPrintIssues (cfgVerbose cfg) useColor termWidth unresolved)
                            putStrLn (summarize useColor unresolved)
                            exitIf cfg unresolved
        else do
            result <- (if cfgCPP cfg then analyzeFileWithCPP
                                     else analyzeFile) (cfgInputFile cfg)
            case result of
                Left err     -> hPutStrLn stderr ("Parse error: " ++ err) >> exitFailure
                Right []     -> putStrLn "No issues found."
                Right issues -> do
                    putStr (prettyPrintIssues (cfgVerbose cfg) useColor termWidth issues)
                    putStrLn (summarize useColor issues)
                    exitIf cfg issues

-- ---------------------------------------------------------------------------
-- Summary statistics
-- ---------------------------------------------------------------------------

-- | One-line summary: total, severity breakdown, category count.
summarize :: Bool -> [Issue] -> String
summarize useColor issues =
    let total    = length issues
        nCrit    = length (filter ((== Critical) . issueSeverity) issues)
        nWarn    = total - nCrit
        nCats    = length (nub (map category issues))
        critStr  = ansiSev useColor Critical (show nCrit ++ " critical")
        warnStr  = ansiSev useColor Warning  (show nWarn ++ " warning")
    in "Found " ++ show total ++ " issue"  ++ plural total
       ++ " (" ++ critStr ++ ", " ++ warnStr ++ ")"
       ++ " across " ++ show nCats ++ " categor" ++ (if nCats == 1 then "y" else "ies")
       ++ "."
  where
    plural 1 = ""
    plural _ = "s"

    ansiSev :: Bool -> Severity -> String -> String
    ansiSev False _   s = s
    ansiSev True  sev s = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"
      where code = case sev of Critical -> "1;31"; Warning -> "1;33"

-- ---------------------------------------------------------------------------
-- Exit code logic
-- ---------------------------------------------------------------------------

-- | Exit non-zero when any critical issue is present, or (with --strict)
--   when any issue at all is present.
exitIf :: Config -> [Issue] -> IO ()
exitIf cfg issues
    | cfgStrict cfg && not (null issues)
        = exitWith (ExitFailure 1)
    | any ((== Critical) . issueSeverity) issues
        = exitWith (ExitFailure 1)
    | otherwise
        = return ()