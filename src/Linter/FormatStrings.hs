{-# LANGUAGE LambdaCase #-}
module Linter.FormatStrings
  ( lintFormatStringsIssues
  , lintDUsedWithSizet
  , lintUUsedWithSizet
  , lintXUsedWithSizet
  , lintDUsedWithPtrdifft
  , lintUUsedWithPtrdifft
  , lintDUsedWithPtr
  , lintUUsedWithPtr
  , lintXUsedWithPtr
  , lintLuUsedForPtrSizedVals
  , lintLdUsedWithLongAssuming64bits
  ) where

import Data.Generics (everywhere, mkT, everything, mkQ)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CString(..))
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Position (posOf)
import Language.C.Data.Ident (Ident(..))
import Analysis.IssueTypes
import Analysis.FormatStrings (fmtArgIndex)
import Linter.Helpers (dispatchLinter)
import Parser.FormatSpecParser (parseLenMod)

-- ---------------------------------------------------------------------------
-- Module-level dispatcher
-- ---------------------------------------------------------------------------

lintFormatStringsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintFormatStringsIssues = dispatchLinter $ \case
    DUsedWithSizet               -> Just lintDUsedWithSizet
    UUsedWithSizet               -> Just lintUUsedWithSizet
    XUsedWithSizet               -> Just lintXUsedWithSizet
    DUsedWithPtrdifft            -> Just lintDUsedWithPtrdifft
    UUsedWithPtrdifft            -> Just lintUUsedWithPtrdifft
    DUsedWithPtr                 -> Just lintDUsedWithPtr
    UUsedWithPtr                 -> Just lintUUsedWithPtr
    XUsedWithPtr                 -> Just lintXUsedWithPtr
    LuUsedForPtrSizedVals        -> Just lintLuUsedForPtrSizedVals
    LdUsedWithLongAssuming64bits -> Just lintLdUsedWithLongAssuming64bits
    _                            -> Nothing

-- ---------------------------------------------------------------------------
-- Per-tag linters
-- (old length-modifier, old conv) -> (new length-modifier, new conv)
-- ---------------------------------------------------------------------------

lintDUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithSizet = tryFixFmtSpec ("", 'd') ("z", 'd')

lintUUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithSizet = tryFixFmtSpec ("", 'u') ("z", 'u')

lintXUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintXUsedWithSizet = tryFixFmtSpec ("", 'x') ("z", 'x')

lintDUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithPtrdifft = tryFixFmtSpec ("", 'd') ("t", 'd')

lintUUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithPtrdifft = tryFixFmtSpec ("", 'u') ("t", 'u')

-- Pointers should use %p; length modifier is dropped, conv changes to 'p'
lintDUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithPtr = tryFixFmtSpec ("", 'd') ("", 'p')

lintUUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithPtr = tryFixFmtSpec ("", 'u') ("", 'p')

lintXUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintXUsedWithPtr = tryFixFmtSpec ("", 'x') ("", 'p')

lintLuUsedForPtrSizedVals :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintLuUsedForPtrSizedVals = tryFixFmtSpec ("l", 'u') ("z", 'u')

-- %ld assumes long is 64-bit; portable replacement is %td (ptrdiff_t)
lintLdUsedWithLongAssuming64bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintLdUsedWithLongAssuming64bits = tryFixFmtSpec ("l", 'd') ("t", 'd')

-- ---------------------------------------------------------------------------
-- AST walking helpers
-- ---------------------------------------------------------------------------

-- | Attempt to fix a format specifier.  If the call site at the issue's
-- position has a string-literal format argument, apply the fix and return
-- @Nothing@ (resolved).  Otherwise return the issue unchanged (unresolved).
tryFixFmtSpec :: (String, Char) -> (String, Char)
              -> CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
tryFixFmtSpec old new ast issue
    | hasPatchableFmt (issuePos issue) ast =
        (fixFmtSpec (issuePos issue) old new ast, Nothing)
    | otherwise = (ast, Just issue)

-- | True when the AST contains a printf-family call at the given position
-- whose format argument is a string literal that can be patched.
hasPatchableFmt :: NodeInfo -> CTranslUnit -> Bool
hasPatchableFmt targetInfo = everything (||) (mkQ False check)
  where
    check :: CExpression NodeInfo -> Bool
    check (CCall callee args info)
        | posOf info == posOf targetInfo
        , CVar (Ident fname _ _) _ <- callee
        , Just idx <- fmtArgIndex fname
        , (_, fmtExpr : _) <- splitAt idx args
        = isStrLit fmtExpr
    check _ = False

    isStrLit (CConst (CStrConst _ _)) = True
    isStrLit _                        = False

-- | Walk the entire AST with 'everywhere', find the CCall at the given source
-- position, and patch the format string literal by replacing the first
-- specifier matching (oldLen, oldConv) with (newLen, newConv).
fixFmtSpec :: NodeInfo
           -> (String, Char)   -- ^ (oldLenMod, oldConv)
           -> (String, Char)   -- ^ (newLenMod, newConv)
           -> CTranslUnit
           -> CTranslUnit
fixFmtSpec targetInfo (oldLen, oldConv) (newLen, newConv) =
    everywhere (mkT fixCall)
  where
    fixCall :: CExpression NodeInfo -> CExpression NodeInfo
    fixCall (CCall callee args info)
        | posOf info == posOf targetInfo
        , CVar (Ident fname _ _) _ <- callee
        , Just idx <- fmtArgIndex fname
        , (pre, fmtExpr : post) <- splitAt idx args
        = CCall callee (pre ++ [patchFmt fmtExpr] ++ post) info
    fixCall expr = expr

    patchFmt (CConst (CStrConst (CString s wide) ni)) =
        CConst (CStrConst (CString (replaceFmtSpecOnce oldLen oldConv newLen newConv s) wide) ni)
    patchFmt e = e

-- ---------------------------------------------------------------------------
-- Format-string specifier replacement
-- ---------------------------------------------------------------------------

-- | Replace the FIRST format specifier in @fmtStr@ whose length modifier and
-- conversion character match @(oldLen, oldConv)@ with @(newLen, newConv@),
-- preserving any flags, width, and precision around the specifier.
-- '%%' escape sequences are skipped without modification.
replaceFmtSpecOnce :: String -> Char -> String -> Char -> String -> String
replaceFmtSpecOnce oldLen oldConv newLen newConv = go False
  where
    go _ [] = []
    -- Skip %% without counting it as a specifier
    go False ('%':'%':rest) = '%' : '%' : go False rest
    go False ('%':rest)     = '%' : replaceSpec rest
    go False (c:rest)       = c   : go False rest
    go _ s                  = s   -- after replacement, copy remaining verbatim

    replaceSpec s =
        let (flags, s1)    = span (`elem` "-+ #0") s
            (width, s2)    = span (`elem` "0123456789") s1
            (prec,  s3)    = case s2 of
                                 ('.':r) -> let (p, r') = span (`elem` "0123456789") r
                                            in ('.' : p, r')
                                 _       -> ("", s2)
            (lenMod, s4)   = parseLenMod s3
        in case s4 of
            []    -> flags ++ width ++ prec ++ lenMod   -- malformed, keep as-is
            (c:r)
                | lenMod == oldLen && c == oldConv ->
                    -- Replace this specifier, then copy the remainder verbatim
                    flags ++ width ++ prec ++ newLen ++ [newConv] ++ r
                | otherwise ->
                    -- Not the specifier we want; keep it and keep looking
                    flags ++ width ++ prec ++ lenMod ++ [c] ++ go False r


