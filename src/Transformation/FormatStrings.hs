module Transformation.FormatStrings where

import Data.Generics (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CString(..))
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Position (posOf)
import Language.C.Data.Ident (Ident(..))
import Analysis.UtilTypes
import Analysis.FormatStrings (fmtArgIndex)

-- ---------------------------------------------------------------------------
-- Module-level dispatcher
-- ---------------------------------------------------------------------------

transformFormatStringsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
transformFormatStringsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      DUsedWithSizet               -> transformDUsedWithSizet               a issue
      UUsedWithSizet               -> transformUUsedWithSizet               a issue
      XUsedWithSizet               -> transformXUsedWithSizet               a issue
      DUsedWithPtrdifft            -> transformDUsedWithPtrdifft            a issue
      UUsedWithPtrdifft            -> transformUUsedWithPtrdifft            a issue
      DUsedWithPtr                 -> transformDUsedWithPtr                 a issue
      UUsedWithPtr                 -> transformUUsedWithPtr                 a issue
      XUsedWithPtr                 -> transformXUsedWithPtr                 a issue
      LuUsedForPtrSizedVals        -> transformLuUsedForPtrSizedVals        a issue
      LdUsedWithLongAssuming64bits -> transformLdUsedWithLongAssuming64bits a issue
      _                            -> (a, Just issue)

-- ---------------------------------------------------------------------------
-- Per-tag transformers
-- (old length-modifier, old conv) -> (new length-modifier, new conv)
-- ---------------------------------------------------------------------------

transformDUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("z", 'd') ast, Nothing)

transformUUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("z", 'u') ast, Nothing)

transformXUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformXUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'x') ("z", 'x') ast, Nothing)

transformDUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithPtrdifft ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("t", 'd') ast, Nothing)

transformUUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithPtrdifft ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("t", 'u') ast, Nothing)

-- Pointers should use %p; length modifier is dropped, conv changes to 'p'
transformDUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformDUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("", 'p') ast, Nothing)

transformUUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformUUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("", 'p') ast, Nothing)

transformXUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformXUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'x') ("", 'p') ast, Nothing)

transformLuUsedForPtrSizedVals :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformLuUsedForPtrSizedVals ast issue =
    (fixFmtSpec (issuePos issue) ("l", 'u') ("z", 'u') ast, Nothing)

-- %ld assumes long is 64-bit; portable replacement is %td (ptrdiff_t)
transformLdUsedWithLongAssuming64bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
transformLdUsedWithLongAssuming64bits ast issue =
    (fixFmtSpec (issuePos issue) ("l", 'd') ("t", 'd') ast, Nothing)

-- ---------------------------------------------------------------------------
-- AST walking helpers
-- ---------------------------------------------------------------------------

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
            (lenMod, s4)   = parseLenMod' s3
        in case s4 of
            []    -> flags ++ width ++ prec ++ lenMod   -- malformed, keep as-is
            (c:r)
                | lenMod == oldLen && c == oldConv ->
                    -- Replace this specifier, then copy the remainder verbatim
                    flags ++ width ++ prec ++ newLen ++ [newConv] ++ r
                | otherwise ->
                    -- Not the specifier we want; keep it and keep looking
                    flags ++ width ++ prec ++ lenMod ++ [c] ++ go False r

parseLenMod' :: String -> (String, String)
parseLenMod' ('h':'h':r) = ("hh", r)
parseLenMod' ('l':'l':r) = ("ll", r)
parseLenMod' ('h':r)     = ("h",  r)
parseLenMod' ('l':r)     = ("l",  r)
parseLenMod' ('j':r)     = ("j",  r)
parseLenMod' ('z':r)     = ("z",  r)
parseLenMod' ('t':r)     = ("t",  r)
parseLenMod' ('L':r)     = ("L",  r)
parseLenMod' ('q':r)     = ("q",  r)
parseLenMod' r           = ("",   r)
