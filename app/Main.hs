module Main where

import System.Environment (getArgs, lookupEnv)
import System.Exit        (exitFailure, exitWith, ExitCode(..))
import System.IO          (hPutStrLn, hIsTerminalDevice, stderr, stdout)
import Text.Read          (readMaybe)
import Data.List          (nub)
import Control.Monad      (when)
import Data.Array         (array, listArray, (!))
import Data.Char          (isAlphaNum)

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
    , cfgDiff       :: Bool
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
    , cfgDiff       = False
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
    go ("--diff":rest)     cfg = go rest cfg { cfgDiff      = True  }
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
    , "  --diff        Print a colored diff of all changes (use with -t or -l)"
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
                    origSrc <- if cfgDiff cfg
                               then do s <- readFile (cfgInputFile cfg)
                                       length s `seq` return s
                               else return ""
                    writeFile outPath src
                    putStrLn ("Transformed output written to: " ++ outPath)
                    when (cfgDiff cfg) $ do
                        putStrLn ("--- " ++ cfgInputFile cfg)
                        putStrLn ("+++ " ++ outPath)
                        printDiff useColor origSrc src
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
                    origSrc <- if cfgDiff cfg
                               then do s <- readFile (cfgInputFile cfg)
                                       length s `seq` return s
                               else return ""
                    writeFile outPath src
                    putStrLn ("Linted output written to: " ++ outPath)
                    when (cfgDiff cfg) $ do
                        putStrLn ("--- " ++ cfgInputFile cfg)
                        putStrLn ("+++ " ++ outPath)
                        printDiff useColor origSrc src
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

-- ---------------------------------------------------------------------------
-- Diff display
-- ---------------------------------------------------------------------------

data Edit    = EKeep String | EDelete String | EInsert String
data TokEdit = TKeep String | TDelete String | TInsert String

isChange :: Edit -> Bool
isChange (EKeep _) = False
isChange _         = True

-- | Compute a line-level edit script comparing two multi-line strings.
--   Uses a standard bottom-up LCS dynamic-programming algorithm.
editScript :: String -> String -> [Edit]
editScript old new_ = lcsDP EKeep EDelete EInsert (lines old) (lines new_)

-- | Compute a token-level edit script between two lists of tokens.
tokEditScript :: [String] -> [String] -> [TokEdit]
tokEditScript = lcsDP TKeep TDelete TInsert

-- | Generic LCS-based edit script over lists of strings.
lcsDP :: (String -> a) -> (String -> a) -> (String -> a)
      -> [String] -> [String] -> [a]
lcsDP keep del ins oldElems newElems = go m n []
  where
    m  = length oldElems
    n  = length newElems
    xa = listArray (1, max 1 m) (oldElems ++ repeat "")
    ya = listArray (1, max 1 n) (newElems ++ repeat "")
    dp = array ((0,0),(m,n)) [ ((i,j), cell i j) | i <- [0..m], j <- [0..n] ]
    cell 0 _ = (0 :: Int)
    cell _ 0 = 0
    cell i j
        | xa ! i == ya ! j = dp ! (i-1,j-1) + 1
        | otherwise        = max (dp ! (i-1,j)) (dp ! (i,j-1))
    go 0 0 acc = acc
    go i 0 acc = go (i-1) 0 (del (xa ! i) : acc)
    go 0 j acc = go 0 (j-1) (ins (ya ! j) : acc)
    go i j acc
        | xa ! i == ya ! j             = go (i-1) (j-1) (keep (xa ! i) : acc)
        | dp ! (i-1,j) > dp ! (i,j-1) = go (i-1) j     (del  (xa ! i) : acc)
        | otherwise                    = go i     (j-1) (ins  (ya ! j) : acc)

-- | Split a C source line into identifier/number runs and single characters.
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
    | isAlphaNum c || c == '_' =
        let (tok, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
        in tok : tokenize rest
    | otherwise = [c] : tokenize cs

-- | Compare two lines token-by-token and annotate changed tokens with ANSI
--   bold (ESC[1m) / normal-weight (ESC[22m).  ESC[22m turns off bold without
--   resetting the current colour, so the caller can wrap the whole line in a
--   colour code and bold will show through cleanly.
intraLineDiff :: Bool -> String -> String -> (String, String)
intraLineDiff False old new_ = (old, new_)
intraLineDiff True  old new_ =
    let edits  = tokEditScript (tokenize old) (tokenize new_)
        oldAnn = concatMap renderOld edits
        newAnn = concatMap renderNew edits
    in (oldAnn, newAnn)
  where
    bold s      = "\ESC[1m" ++ s ++ "\ESC[22m"
    renderOld (TKeep s)   = s
    renderOld (TDelete s) = bold s
    renderOld (TInsert _) = ""
    renderNew (TKeep s)   = s
    renderNew (TDelete _) = ""
    renderNew (TInsert s) = bold s

type AnnLine = (Maybe Int, Maybe Int, Edit)

-- | Annotate each edit with the old and new line numbers.
annotateEdits :: [Edit] -> [AnnLine]
annotateEdits = go 1 1
  where
    go _ _ []                   = []
    go oi ni (EKeep s   : rest) = (Just oi, Just ni, EKeep s)   : go (oi+1) (ni+1) rest
    go oi ni (EDelete s : rest) = (Just oi, Nothing, EDelete s) : go (oi+1) ni     rest
    go oi ni (EInsert s : rest) = (Nothing, Just ni, EInsert s) : go oi     (ni+1) rest

diffCtx :: Int
diffCtx = 3

-- | Split annotated edits into hunks, each with up to 'diffCtx' context lines.
splitHunks :: [AnnLine] -> [[AnnLine]]
splitHunks ann
    | null changed = []
    | otherwise    = [ [ arr ! j | j <- [lo..hi] ] | (lo, hi) <- windows ]
  where
    total   = length ann
    arr     = listArray (0, max 0 (total-1)) ann
    changed = [ i | (i, (_,_,e)) <- zip [0..] ann, isChange e ]
    ranges  = [ (max 0 (i - diffCtx), min (total-1) (i + diffCtx)) | i <- changed ]
    windows = mergeRanges ranges

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges []     = []
mergeRanges (r:rs) = go r rs
  where
    go cur []                         = [cur]
    go (lo, hi) ((lo2, hi2) : rest)
        | lo2 <= hi + 1               = go (lo, max hi hi2) rest
        | otherwise                   = (lo, hi) : go (lo2, hi2) rest

-- | Print a colored unified-style diff of two multi-line strings to stdout.
printDiff :: Bool -> String -> String -> IO ()
printDiff useColor old new_ = do
    let edits = editScript old new_
        ann   = annotateEdits edits
        hs    = splitHunks ann
        numW  = length (show (max (length (lines old)) (length (lines new_))))
    if null hs
        then putStrLn (diffColor useColor "2" "(no changes)")
        else mapM_ (printHunk useColor numW) hs

printHunk :: Bool -> Int -> [AnnLine] -> IO ()
printHunk useColor numW hLines = do
    let oldNums  = [ n | (Just n, _,      _) <- hLines ]
        newNums  = [ n | (_,      Just n, _) <- hLines ]
        oldStart = if null oldNums then 1 else minimum oldNums
        newStart = if null newNums then 1 else minimum newNums
        oldCount = length oldNums
        newCount = length newNums
        header   = "@@ -" ++ show oldStart ++ "," ++ show oldCount
                        ++ " +" ++ show newStart ++ "," ++ show newCount ++ " @@"
    putStrLn (diffColor useColor "36;1" header)
    printAnnLines useColor numW hLines

-- | Print annotated lines, grouping consecutive Delete/Insert blocks and
--   applying token-level intra-line bold diffs to similar pairs.
printAnnLines :: Bool -> Int -> [AnnLine] -> IO ()
printAnnLines _ _ [] = return ()
printAnnLines useColor numW ls =
    let (dels, rest1) = span isDelLine ls
        (inss, rest2) = span isInsLine rest1
    in if null dels && null inss
       then do
           putStrLn (renderAnnLine useColor numW (head ls))
           printAnnLines useColor numW (tail ls)
       else do
           let nPairs    = min (length dels) (length inss)
               paired    = take nPairs (zip dels inss)
               extraDels = drop nPairs dels
               extraInss = drop nPairs inss
               annotate (oi, _, del) (_, ni, ins) =
                   let old  = case del of { EDelete s -> s; _ -> "" }
                       new_ = case ins of { EInsert s -> s; _ -> "" }
                       (oldAnn, newAnn)
                         | tokenSim old new_ > 0.3 = intraLineDiff useColor old new_
                         | otherwise               = (old, new_)
                   in (oi, ni, oldAnn, newAnn)
               annotated = map (uncurry annotate) paired
           -- All deletes first (standard unified-diff order)
           mapM_ (\(oi, _, oldAnn, _) ->
               putStrLn (renderLine useColor numW oi Nothing "31" "-" oldAnn)) annotated
           mapM_ (putStrLn . renderAnnLine useColor numW) extraDels
           -- All inserts after
           mapM_ (\(_, ni, _, newAnn) ->
               putStrLn (renderLine useColor numW Nothing ni "32" "+" newAnn)) annotated
           mapM_ (putStrLn . renderAnnLine useColor numW) extraInss
           printAnnLines useColor numW rest2
  where
    isDelLine (_, _, EDelete _) = True
    isDelLine _                 = False
    isInsLine (_, _, EInsert _) = True
    isInsLine _                 = False

-- | Dice-coefficient similarity between two strings based on shared tokens.
--   Returns a value in [0,1]; 1.0 means identical.
tokenSim :: String -> String -> Double
tokenSim s1 s2 =
    let ts1   = tokenize s1
        ts2   = tokenize s2
        edits = tokEditScript ts1 ts2
        nKeep = length [ () | TKeep _ <- edits ]
        total = length ts1 + length ts2
    in if total == 0 then 1.0
       else fromIntegral (2 * nKeep) / fromIntegral total

-- | Render one annotated line as a string (used for unpaired edits and context).
renderAnnLine :: Bool -> Int -> AnnLine -> String
renderAnnLine useColor numW (mOld, mNew, edit) =
    renderLine useColor numW mOld mNew colorCode pfx content
  where
    (colorCode, pfx, content) = case edit of
        EKeep s   -> ("0",  " ", s)
        EDelete s -> ("31", "-", s)
        EInsert s -> ("32", "+", s)

-- | Format one diff output line with padded line numbers, a prefix sigil,
--   and (optionally ANSI-annotated) content.
renderLine :: Bool -> Int -> Maybe Int -> Maybe Int
           -> String -> String -> String -> String
renderLine useColor numW mOld mNew colorCode pfx content =
    diffColor useColor colorCode
        (padL numW (maybe "" show mOld) ++ " "
         ++ padL numW (maybe "" show mNew) ++ " "
         ++ pfx ++ " " ++ content)
  where
    padL n s = replicate (n - length s) ' ' ++ s

diffColor :: Bool -> String -> String -> String
diffColor False _    s = s
diffColor True  code s = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"