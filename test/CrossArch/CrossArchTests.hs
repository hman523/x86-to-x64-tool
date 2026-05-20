{-# LANGUAGE ScopedTypeVariables #-}
-- | Cross-architecture tests for semantic equivalence.
--
-- For every C program in @test\/c_progs\/cross_arch\/@, the test:
--
--   1. Compiles & runs it as 32-bit (@gcc -m32@).
--   2. Compiles & runs it as 64-bit (@gcc -m64@) — output should differ.
--   3. Transforms the source with our tool.
--   4. Compiles & runs the transformed source as 64-bit — output should
--      match the 32-bit original.
--
-- Compilation and execution happen inside a Podman container
-- (@x86-to-x64-test-env@, built from @Dockerfile.test-env@ at the project
-- root) so that cross-compilation with @gcc -m32@ is available even on
-- macOS / ARM hosts.
--
-- All container compilations are batched into just two @podman exec@ calls
-- (one for all original m32\/m64 compilations, one for all transformed
-- m64 compilations) to minimise overhead.
--
-- If Podman is not available the entire test group is skipped with
-- @pendingWith@, so a normal @cabal test@ still succeeds.
module CrossArch.CrossArchTests (crossArchSpec) where

import Test.Hspec
import System.Exit        (ExitCode(..))
import System.Process     (readProcessWithExitCode)
import System.Directory   (getCurrentDirectory, listDirectory,
                           doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath    (takeBaseName)
import Data.List          (isSuffixOf, sort)
import Data.Char          (isSpace)
import Control.Exception  (catch, IOException)
import Control.Monad      (when, forM_)
import Data.IORef

import X86_to_X64         (transformFile)

-- | Podman image tag used for the test environment.
imageName :: String
imageName = "x86-to-x64-test-env"

-- | Name of the long-lived container used for all cross-arch tests.
containerName :: String
containerName = "x86-to-x64-test-runner"

-- | Base directory (relative to project root / container workdir) where
--   per-file compilation results are written by batch scripts.
resultsDirBase :: String
resultsDirBase = "test/c_progs/cross_arch/.results"

-- | Return a results-directory path that is unique to this OS process,
--   so that concurrent @cabal test@ invocations do not share result files.
makeResultsDir :: IO String
makeResultsDir = do
    (_, pidOut, _) <- readProcessWithExitCode "sh" ["-c", "echo $PPID"] ""
    return $ resultsDirBase ++ "." ++ strip pidOut

-- | Trim leading/trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Read a file strictly (force full contents before returning).
readStrict :: FilePath -> IO String
readStrict path = do
    s <- readFile path
    length s `seq` return s

-- ---------------------------------------------------------------------------
-- Podman helpers
-- ---------------------------------------------------------------------------

-- | Check whether @podman@ is on PATH and responsive.
podmanAvailable :: IO Bool
podmanAvailable =
    (do (ec, _, _) <- readProcessWithExitCode "podman" ["info"] ""
        return (ec == ExitSuccess))
    `catch` (\(_ :: IOException) -> return False)

-- | Build the test-env image if it does not already exist.
ensureImage :: FilePath -> IO ()
ensureImage projectRoot = do
    (_, out, _) <- readProcessWithExitCode
        "podman" ["images", "-q", imageName] ""
    when (null (strip out)) $ do
            (ec, _, _err) <- readProcessWithExitCode
                "podman"
                [ "build"
                , "--platform", "linux/amd64"
                , "-t", imageName
                , "-f", projectRoot ++ "/Dockerfile.test-env"
                , projectRoot
                ]
                ""
            ec `shouldBe` ExitSuccess

-- | Start a long-lived container for the duration of the test suite.
--   If a container from a previous run is still running, reuse it.
startContainer :: FilePath -> IO ()
startContainer projectRoot = do
    -- Check if the container is already running (from a previous test run).
    (_, out, _) <- readProcessWithExitCode "podman"
        ["inspect", "-f", "{{.State.Running}}", containerName] ""
    if strip out == "true"
        then return ()  -- Reuse the already-running container.
        else do
            -- Remove any stopped leftover container.
            _ <- readProcessWithExitCode "podman"
                ["rm", "-f", containerName] ""
            (ec, _, err) <- readProcessWithExitCode "podman"
                [ "run", "-d"
                , "--name", containerName
                , "--platform", "linux/amd64"
                , "-v", projectRoot ++ ":/work"
                , "-w", "/work"
                , imageName
                , "sleep", "infinity"
                ]
                ""
            case ec of
                ExitSuccess -> return ()
                _           -> error $ "Failed to start test container: " ++ err

-- | Leave the container running for reuse on subsequent test runs.
--   The container is lightweight (just @sleep infinity@) and uses
--   negligible resources when idle.
stopContainer :: IO ()
stopContainer = return ()

-- | Run a shell command inside the already-running container.
--   Returns @(exitCode, stdout, stderr)@.
podmanExec :: String -> IO (ExitCode, String, String)
podmanExec cmd =
    readProcessWithExitCode "podman"
        [ "exec", containerName
        , "sh", "-c", cmd
        ]
        ""

-- | Remove the results directory from the bind mount.
cleanupResults :: FilePath -> IO ()
cleanupResults dir = do
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir

-- ---------------------------------------------------------------------------
-- Batch compilation helpers
-- ---------------------------------------------------------------------------

-- | Shell fragment: compile @file@ with @flags@, run the binary, and write
--   stdout to @outFile@ and the exit code to @ecFile@.
--   Uses a unique binary path derived from @tag@ to avoid conflicts in
--   parallel execution.
compileFragment :: String -> String -> String -> String -> String -> String
compileFragment file flags outFile ecFile tag = concat
    [ "{ gcc ", flags, " -o /tmp/prog_", tag, " ", file
    , " && /tmp/prog_", tag, "; } > "
    , outFile, " 2>&1; printf '%d' $? > ", ecFile, " & "
    ]

-- | Batch compile+run every file as both 32-bit and 64-bit in a single exec.
--   All compilations run in parallel.  Results are written to files under
--   @rd@ on the bind mount.
batchCompileOriginals :: String -> [FilePath] -> IO ()
batchCompileOriginals rd files = do
    let script = "mkdir -p " ++ rd ++ "; " ++ concatMap oneFile files
                 ++ "wait"
        oneFile f = let b = takeBaseName f
                    in  compileFragment f "-m32" (rp b ".m32.out") (rp b ".m32.ec") (b ++ "_32")
                     ++ compileFragment f "-m64" (rp b ".m64.out") (rp b ".m64.ec") (b ++ "_64")
        rp base suffix = rd ++ "/" ++ base ++ suffix
    _ <- podmanExec script
    return ()

-- | Batch compile+run every transformed file as 64-bit in a single exec.
--   All compilations run in parallel.
--   Takes a list of @(baseName, transformedRelPath)@ pairs.
batchCompileTransformed :: String -> [(String, FilePath)] -> IO ()
batchCompileTransformed rd pairs = do
    let script = concatMap oneFile pairs ++ "wait"
        oneFile (b, f) = concat
            [ "{ gcc -m64 -o /tmp/prog_tr_", b, " ", f, " 2>", rp b ".tr.err"
            , " && /tmp/prog_tr_", b, " > ", rp b ".tr.out", " 2>&1; "
            , "printf '%d' $? > ", rp b ".tr.ec", "; } & "
            ]
        rp base suffix = rd ++ "/" ++ base ++ suffix
    _ <- podmanExec script
    return ()

-- ---------------------------------------------------------------------------
-- Batch test orchestration
-- ---------------------------------------------------------------------------

-- | Run all compilation phases and store per-file transform results in the
--   supplied 'IORef'.  After this returns, result files exist on disk for
--   every test file and the 'IORef' holds the transform outcome.
runBatchPhases :: FilePath
              -> String
              -> [FilePath]
              -> IORef [(FilePath, Either String ())]
              -> IO ()
runBatchPhases projectRoot rd cFiles transformRef = do
    startContainer projectRoot

    -- Phase 1: batch compile+run all originals (m32 + m64) — one exec.
    batchCompileOriginals rd cFiles

    -- Phase 2: transform every file on the host (no container needed).
    trs <- mapM (\f -> do
        let absPath = projectRoot ++ "/" ++ f
        r <- transformFile absPath
        case r of
            Right transformed -> do
                let transformedPath = f ++ ".x64.c"
                writeFile (projectRoot ++ "/" ++ transformedPath) transformed
                return (f, Right ())
            Left err -> return (f, Left err)
        ) cFiles
    writeIORef transformRef trs

    -- Phase 3: batch compile+run all successful transformations — one exec.
    let pairs = [ (takeBaseName f, f ++ ".x64.c") | (f, Right _) <- trs ]
    when (not (null pairs)) $ batchCompileTransformed rd pairs

-- ---------------------------------------------------------------------------
-- Top-level spec
-- ---------------------------------------------------------------------------

crossArchSpec :: Spec
crossArchSpec = describe "Cross-architecture semantic equivalence" $ do
    -- Check Podman availability at the start; skip the whole group if missing.
    hasPodman <- runIO podmanAvailable
    if not hasPodman
        then it "requires Podman (skipped)" $
                 pendingWith "Podman not available – skipping cross-arch tests"
        else do
            projectRoot <- runIO getCurrentDirectory

            -- Build/ensure the Podman image before running any test.
            runIO $ ensureImage projectRoot

            -- Discover all .c files in the cross_arch directory.
            let crossArchDir = "test/c_progs/cross_arch"
            cFiles <- runIO $ do
                entries <- listDirectory (projectRoot ++ "/" ++ crossArchDir)
                return $ sort
                    [ crossArchDir ++ "/" ++ f
                    | f <- entries
                    , ".c" `isSuffixOf` f
                    , not (".x64.c" `isSuffixOf` f)
                    ]

            -- Generate a unique results directory for this test run so that
            -- concurrent @cabal test@ invocations do not share result files.
            rd <- runIO makeResultsDir

            -- IORef to hold per-file transform outcomes (filled by runBatchPhases).
            transformRef <- runIO $ newIORef []

            -- Run all container work up-front; tear down on completion.
            beforeAll_ (runBatchPhases projectRoot rd cFiles transformRef) $
              afterAll_ (stopContainer >> cleanupResults (projectRoot ++ "/" ++ rd)) $ do

                forM_ cFiles $ \relPath ->
                    it (relPath ++ " preserves 32-bit behaviour after transformation") $ do
                        let b   = takeBaseName relPath
                            readResult sfx = readStrict
                                (projectRoot ++ "/" ++ rd ++ "/" ++ b ++ sfx)
                            toEC s = if strip s == "0" then ExitSuccess
                                                       else ExitFailure 1

                        -- Check step 1 (32-bit) result
                        ec32 <- toEC <$> readResult ".m32.ec"
                        ec32 `shouldBe` ExitSuccess
                        out32 <- strip <$> readResult ".m32.out"

                        -- Check step 2 (64-bit untransformed) result
                        ec64 <- toEC <$> readResult ".m64.ec"
                        ec64 `shouldBe` ExitSuccess
                        out64 <- strip <$> readResult ".m64.out"

                        out32 `shouldNotBe` out64

                        -- Check step 3 (transform) + step 4 (transformed 64-bit)
                        trs <- readIORef transformRef
                        case lookup relPath trs of
                            Just (Left err) -> expectationFailure $
                                "Transformation failed for " ++ relPath ++ ": " ++ err
                            Just (Right ()) -> do
                                ecTr <- toEC <$> readResult ".tr.ec"
                                case ecTr of
                                    ExitSuccess -> do
                                        outTr <- strip <$> readResult ".tr.out"
                                        outTr `shouldBe` out32
                                    _ -> do
                                        errTr <- readResult ".tr.err"
                                        expectationFailure $
                                            "Compilation of transformed file failed:\n" ++ errTr
                            Nothing -> expectationFailure
                                "Internal error: no transform result found"
