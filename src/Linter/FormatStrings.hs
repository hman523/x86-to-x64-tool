module Linter.FormatStrings where

import Data.Generics (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CString(..))
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Position (posOf)
import Language.C.Data.Ident (Ident(..))
import Analysis.IssueTypes
import Analysis.FormatStrings (fmtArgIndex)
import Parser.FormatSpecParser (parseLenMod)

-- ---------------------------------------------------------------------------
-- Module-level dispatcher
-- ---------------------------------------------------------------------------

lintFormatStringsIssues :: CTranslUnit -> [Issue] -> (CTranslUnit, [Issue])
lintFormatStringsIssues ast issues = foldl applyOne (ast, []) issues
  where
    applyOne (a, unresolved) issue =
      let (a', mi) = dispatch a issue
      in (a', maybe unresolved (: unresolved) mi)

    dispatch a issue = case issueType issue of
      DUsedWithSizet               -> lintDUsedWithSizet               a issue
      UUsedWithSizet               -> lintUUsedWithSizet               a issue
      XUsedWithSizet               -> lintXUsedWithSizet               a issue
      DUsedWithPtrdifft            -> lintDUsedWithPtrdifft            a issue
      UUsedWithPtrdifft            -> lintUUsedWithPtrdifft            a issue
      DUsedWithPtr                 -> lintDUsedWithPtr                 a issue
      UUsedWithPtr                 -> lintUUsedWithPtr                 a issue
      XUsedWithPtr                 -> lintXUsedWithPtr                 a issue
      LuUsedForPtrSizedVals        -> lintLuUsedForPtrSizedVals        a issue
      LdUsedWithLongAssuming64bits -> lintLdUsedWithLongAssuming64bits a issue
      _                            -> (a, Just issue)

-- ---------------------------------------------------------------------------
-- Per-tag linters
-- (old length-modifier, old conv) -> (new length-modifier, new conv)
-- ---------------------------------------------------------------------------

lintDUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("z", 'd') ast, Nothing)

lintUUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("z", 'u') ast, Nothing)

lintXUsedWithSizet :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintXUsedWithSizet ast issue =
    (fixFmtSpec (issuePos issue) ("", 'x') ("z", 'x') ast, Nothing)

lintDUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithPtrdifft ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("t", 'd') ast, Nothing)

lintUUsedWithPtrdifft :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithPtrdifft ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("t", 'u') ast, Nothing)

-- Pointers should use %p; length modifier is dropped, conv changes to 'p'
lintDUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintDUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'd') ("", 'p') ast, Nothing)

lintUUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintUUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'u') ("", 'p') ast, Nothing)

lintXUsedWithPtr :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintXUsedWithPtr ast issue =
    (fixFmtSpec (issuePos issue) ("", 'x') ("", 'p') ast, Nothing)

lintLuUsedForPtrSizedVals :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintLuUsedForPtrSizedVals ast issue =
    (fixFmtSpec (issuePos issue) ("l", 'u') ("z", 'u') ast, Nothing)

-- %ld assumes long is 64-bit; portable replacement is %td (ptrdiff_t)
lintLdUsedWithLongAssuming64bits :: CTranslUnit -> Issue -> (CTranslUnit, Maybe Issue)
lintLdUsedWithLongAssuming64bits ast issue =
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


