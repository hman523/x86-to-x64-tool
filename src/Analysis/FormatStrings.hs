module Analysis.FormatStrings where

import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CString(..))
import Language.C.Data.Node
import Language.C.Data.Ident
import Analysis.UtilTypes
import Analysis.TypeChecker
import qualified Data.Map as Map

analyzeFormatStringIssues :: CTranslUnit -> [Issue]
analyzeFormatStringIssues ast@(CTranslUnit decls _) =
    let tenv = buildTypedefEnv ast
    in concatMap (analyzeDecl (checkFormatCall tenv) Map.empty) decls

-- N.B. Individual export stubs are kept so existing call-sites still compile.
checkdUsedWithSizet         :: CTranslUnit -> [Issue]
checkuUsedWithSizet         :: CTranslUnit -> [Issue]
checkxUsedWithSizet         :: CTranslUnit -> [Issue]
checkdUsedWithPtrdifft      :: CTranslUnit -> [Issue]
checkuUsedWithPtrdifft      :: CTranslUnit -> [Issue]
checkdUsedWithPtr           :: CTranslUnit -> [Issue]
checkuUsedWithPtr           :: CTranslUnit -> [Issue]
checkxUsedWithPtr           :: CTranslUnit -> [Issue]
checkluUsedForPtrSizedVals  :: CTranslUnit -> [Issue]
checkldUsedWithLongAssuming64bits :: CTranslUnit -> [Issue]

checkdUsedWithSizet         = filterTag DUsedWithSizet
checkuUsedWithSizet         = filterTag UUsedWithSizet
checkxUsedWithSizet         = filterTag XUsedWithSizet
checkdUsedWithPtrdifft      = filterTag DUsedWithPtrdifft
checkuUsedWithPtrdifft      = filterTag UUsedWithPtrdifft
checkdUsedWithPtr           = filterTag DUsedWithPtr
checkuUsedWithPtr           = filterTag UUsedWithPtr
checkxUsedWithPtr           = filterTag XUsedWithPtr
checkluUsedForPtrSizedVals  = filterTag LuUsedForPtrSizedVals
checkldUsedWithLongAssuming64bits = filterTag LdUsedWithLongAssuming64bits

filterTag :: IssueTag -> CTranslUnit -> [Issue]
filterTag tag ast = filter ((== tag) . issueType) (analyzeFormatStringIssues ast)

-- ---------------------------------------------------------------------------
-- Format-specifier parser
-- ---------------------------------------------------------------------------

data FmtSpec = FmtSpec
    { fmtLenMod :: String  -- "", "h", "l", "ll", "j", "z", "t", "L"
    , fmtConv   :: Char    -- 'd', 'u', 'x', 'o', 's', 'p', 'f', etc.
    } deriving (Show, Eq)

-- | Scan a printf-style format string and return its format specifiers.
parseFormatSpecs :: String -> [FmtSpec]
parseFormatSpecs []              = []
parseFormatSpecs ('%':'%':rest)  = parseFormatSpecs rest
parseFormatSpecs ('%':rest)      = parseSpec rest
parseFormatSpecs (_:rest)        = parseFormatSpecs rest

parseSpec :: String -> [FmtSpec]
parseSpec s =
    let s1 = dropWhile (`elem` "-+ #0") s          -- flags
        s2 = dropWhile (`elem` "0123456789") s1     -- width (skips * too)
        s3 = case s2 of                              -- precision
                 ('.':r) -> dropWhile (`elem` "0123456789") r
                 _       -> s2
        (lenMod, s4) = parseLenMod s3
    in case s4 of
        []    -> []
        (c:r) -> FmtSpec lenMod c : parseFormatSpecs r

parseLenMod :: String -> (String, String)
parseLenMod ('h':'h':r) = ("hh", r)
parseLenMod ('l':'l':r) = ("ll", r)
parseLenMod ('h':r)     = ("h",  r)
parseLenMod ('l':r)     = ("l",  r)
parseLenMod ('j':r)     = ("j",  r)
parseLenMod ('z':r)     = ("z",  r)
parseLenMod ('t':r)     = ("t",  r)
parseLenMod ('L':r)     = ("L",  r)
parseLenMod ('q':r)     = ("q",  r)
parseLenMod r           = ("",   r)

-- ---------------------------------------------------------------------------
-- Call-site analysis
-- ---------------------------------------------------------------------------

-- | Index of the format-string argument for known printf-family functions.
fmtArgIndex :: String -> Maybe Int
fmtArgIndex "printf"    = Just 0
fmtArgIndex "fprintf"   = Just 1
fmtArgIndex "dprintf"   = Just 1
fmtArgIndex "sprintf"   = Just 1
fmtArgIndex "snprintf"  = Just 2
fmtArgIndex "vprintf"   = Just 0
fmtArgIndex "vfprintf"  = Just 1
fmtArgIndex "vsprintf"  = Just 1
fmtArgIndex "vsnprintf" = Just 2
fmtArgIndex _            = Nothing

-- | Extract the literal string content of a CConst string expression.
getFmtString :: CExpression a -> Maybe String
getFmtString (CConst (CStrConst (CString s _) _)) = Just s
getFmtString _                                      = Nothing

-- | Check a single call expression for format-string issues.
checkFormatCall :: TypedefEnv -> TypeEnv -> CExpression NodeInfo -> [Issue]
checkFormatCall tenv env (CCall (CVar (Ident fname _ _) _) args info) =
    case fmtArgIndex fname of
        Nothing  -> []
        Just idx ->
            case splitAt idx args of
                (_, [])             -> []
                (_, fmtExpr:vArgs)  ->
                    case getFmtString fmtExpr of
                        Nothing     -> []
                        Just fmtStr ->
                            let specs    = parseFormatSpecs fmtStr
                                argTypes = map (resolveTypedef tenv . typeOfExpr env) vArgs
                            in concatMap (checkSpecAndType info) (zip specs argTypes)
checkFormatCall _ _ _ = []

checkSpecAndType :: NodeInfo -> (FmtSpec, CType) -> [Issue]
checkSpecAndType info (FmtSpec lenMod conv, argType) =
    [ createIssue info Warning tag | (True, tag) <- matchRules ]
  where
    isSizet   t = t == TULong || t == TUInt
    isPtrdiff t = t == TLong
    isPtr       = isPointer argType

    matchRules =
        [ (lenMod == ""  && conv == 'd' && isSizet argType,      DUsedWithSizet)
        , (lenMod == ""  && conv == 'u' && isSizet argType,      UUsedWithSizet)
        , (lenMod == ""  && conv == 'x' && isSizet argType,      XUsedWithSizet)
        , (lenMod == ""  && conv == 'd' && isPtrdiff argType,    DUsedWithPtrdifft)
        , (lenMod == ""  && conv == 'u' && isPtrdiff argType,    UUsedWithPtrdifft)
        , (lenMod == ""  && conv == 'd' && isPtr,                DUsedWithPtr)
        , (lenMod == ""  && conv == 'u' && isPtr,                UUsedWithPtr)
        , (lenMod == ""  && conv == 'x' && isPtr,                XUsedWithPtr)
        , (lenMod == "l" && conv == 'u' && isPtr,                LuUsedForPtrSizedVals)
        , (lenMod == "l" && conv == 'd' && isLongType' argType,  LdUsedWithLongAssuming64bits)
        ]