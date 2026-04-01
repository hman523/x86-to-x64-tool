module Main where

import System.Environment (getArgs, lookupEnv)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, hIsTerminalDevice, stderr, stdout)
import Text.Read          (readMaybe)

import X86_to_X64         (analyzeFile, transformFile, prettyPrintIssues)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data Config = Config
    { cfgInputFile  :: FilePath
    , cfgOutputFile :: Maybe FilePath
    , cfgVerbose    :: Bool
    , cfgTransform  :: Bool
    , cfgNoColor    :: Bool
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cfgInputFile  = ""
    , cfgOutputFile = Nothing
    , cfgVerbose    = False
    , cfgTransform  = False
    , cfgNoColor    = False
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
        | otherwise               = Right cfg
    go ("-h":_)          _   = Left ""
    go ("--help":_)      _   = Left ""
    go ("-v":rest)         cfg = go rest cfg { cfgVerbose   = True  }
    go ("-t":rest)         cfg = go rest cfg { cfgTransform = True  }
    go ("--no-color":rest) cfg = go rest cfg { cfgNoColor   = True  }
    go ("-o":path:rest)  cfg = go rest cfg { cfgOutputFile = Just path }
    go ("-o":[])         _   = Left "-o requires an output file path argument."
    go (path:rest)       cfg
        | not (null (cfgInputFile cfg)) = Left ("Unexpected argument: " ++ path)
        | otherwise                     = go rest cfg { cfgInputFile = path }

usageMessage :: String
usageMessage = unlines
    [ "Usage: x86-to-x64-tool <file.c> [options]"
    , ""
    , "Options:"
    , "  -v           Verbose: print a one-sentence explanation for each issue"
    , "  -t           Transform: apply automated x86-to-x64 transformations"
    , "  -o <file>    Write transformed source to this file (default: <input>.x64.c)"
    , "  --no-color   Disable colored output"
    , "  -h, --help   Show this help message"
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
            result <- transformFile (cfgInputFile cfg)
            case result of
                Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> exitFailure
                Right (src, unresolved) -> do
                    let outPath = case cfgOutputFile cfg of
                                      Just p  -> p
                                      Nothing -> cfgInputFile cfg ++ ".x64.c"
                    writeFile outPath src
                    putStrLn ("Transformed output written to: " ++ outPath)
                    if null unresolved
                        then putStrLn "All issues resolved."
                        else do
                            putStrLn (show (length unresolved) ++ " issue(s) could not be automatically resolved:")
                            putStr (prettyPrintIssues (cfgVerbose cfg) useColor termWidth unresolved)
        else do
            result <- analyzeFile (cfgInputFile cfg)
            case result of
                Left err     -> hPutStrLn stderr ("Parse error: " ++ err) >> exitFailure
                Right []     -> putStrLn "No issues found."
                Right issues -> putStr (prettyPrintIssues (cfgVerbose cfg) useColor termWidth issues)