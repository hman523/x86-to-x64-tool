-- | Post-transform format-string fixup pass.
--
--   After 'Transformer.LongReplacement.transformLongs' rewrites @long@
--   / @unsigned long@ variables to fixed-width types such as @int32_t@ and
--   @uint32_t@, any @printf@-family format specifier that was written as @%ld@
--   or @%lu@ for what was a 32-bit @long@ is now mismatched:
--
--   * @int32_t@ is 4 bytes; @%ld@ reads a 64-bit @long@  → UB on LP64.
--   * @uint32_t@ is 4 bytes; @%lu@ reads a 64-bit @unsigned long@ → UB.
--
--   This pass corrects those specifiers.  For each @printf@-family call whose
--   format argument is a string literal, it aligns the format specifiers with
--   the variadic arguments and, for any @%ld@ / @%lu@ argument that is a
--   simple variable present in the 'RetypeMap', replaces the specifier with
--   the one appropriate for the new type:
--
--   @long@ variable after retype:
--     * @int32_t@   → @%ld@ / @%td@ / @%zd@ become @%d@
--     * @uint32_t@  → @%lu@ / @%tu@ / @%zd@ become @%u@
--     * @ptrdiff_t@ → @%d@ / @%ld@ become @%td@
--     * @size_t@    → @%d@ / @%lu@ become @%zu@
--     * @intptr_t@  → @%d@ becomes @%ld@ (correct on LP64; already correct if @%ld@)
--     * @uintptr_t@ → @%u@ becomes @%lu@ (correct on LP64)
--
--   Only specifiers directly aligned with a retyped variable (plain @CVar@
--   argument) are modified.  Expressions, casts, and non-@%ld@/@%lu@
--   specifiers are left untouched.
module Transformer.FormatFix
    ( fixFormatStrings
    ) where

import qualified Data.Map.Strict as Map

import Data.Generics               (everywhere, mkT)
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CString(..))
import Language.C.Data.Node        (NodeInfo)
import Language.C.Data.Ident       (Ident(..))

import Analysis.FormatStrings      (fmtArgIndex, getFmtString)
import Transformer.LongReplacement (RetypeMap)

-- | Fix @%ld@ / @%lu@ format specifiers that are now mismatched after the
--   long-replacement pass.
fixFormatStrings :: RetypeMap -> CTranslUnit -> CTranslUnit
fixFormatStrings rmap = everywhere (mkT fixCall)
  where
    fixCall :: CExpression NodeInfo -> CExpression NodeInfo
    fixCall (CCall fn@(CVar (Ident fname _ _) _) args ni) =
        case fmtArgIndex fname of
            Nothing  -> CCall fn args ni
            Just idx ->
                case splitAt idx args of
                    (_, [])            -> CCall fn args ni
                    (pre, fmtExpr:vArgs) ->
                        case getFmtString fmtExpr of
                            Nothing     -> CCall fn args ni
                            Just fmtStr ->
                                let argNames = map varName vArgs ++ repeat Nothing
                                    fmtStr'  = fixFmt fmtStr argNames
                                in CCall fn (pre ++ setFmtString fmtExpr fmtStr' : vArgs) ni
    fixCall e = e

    varName :: CExpression NodeInfo -> Maybe String
    varName (CVar (Ident n _ _) _) = Just n
    varName _                       = Nothing

    -- | Walk the format string character by character, replacing @%ld@ /
    --   @%lu@ tokens whose argument is a retyped variable.
    fixFmt :: String -> [Maybe String] -> String
    fixFmt []          _       = []
    fixFmt ('%':'%':r) ms      = '%' : '%' : fixFmt r ms
    fixFmt ('%':r)     (m:ms)  =
        let (rawSpec, after) = consumeSpec r
        in '%' : patchSpec rawSpec m ++ fixFmt after ms
    fixFmt ('%':r)     []      =
        let (rawSpec, after) = consumeSpec r
        in '%' : rawSpec ++ fixFmt after []
    fixFmt (c:r)       ms      = c : fixFmt r ms

    -- | Patch one raw specifier (without leading @%@) by normalising the
    --   length modifier to match the variable's new type in the 'RetypeMap'.
    --
    --   This handles both directions:
    --   * @%ld@ / @%lu@ for a variable now typed @int32_t@ / @uint32_t@
    --     → strip the @l@ modifier → @%d@ / @%u@
    --   * @%td@ / @%zd@ introduced by the linter for a variable the
    --     transformer later classifies as @int32_t@ / @uint32_t@
    --     → strip the spurious length modifier → @%d@ / @%u@
    --   * @%d@ / @%u@ for a variable now typed @ptrdiff_t@ / @size_t@
    --     → add the correct length modifier → @%td@ / @%zd@
    --
    --   Flags, width and precision are always preserved.
    patchSpec :: String -> Maybe String -> String
    patchSpec rawSpec mName =
        case mName >>= (`Map.lookup` rmap) >>= typedefName of
            Nothing -> rawSpec
            Just nm ->
                let (prefix, lmod, conv) = decompSpec rawSpec
                in if conv `elem` "diouxX"
                   then let newLmod = correctLmod nm
                        in if newLmod == lmod
                           then rawSpec
                           else prefix ++ newLmod ++ [conv]
                   else rawSpec

    -- | Decompose a raw specifier (no leading @%@) into
    --   @(flags++width++prec, length_modifier, conversion_char)@.
    decompSpec :: String -> (String, String, Char)
    decompSpec s =
        let (flags, s1) = span (`elem` "-+ #0")      s
            (width, s2) = span (`elem` "0123456789") s1
            (prec,  s3) = case s2 of
                              '.':r -> let (p, r') = span (`elem` "0123456789") r
                                       in ('.' : p, r')
                              _     -> ("", s2)
            (lmod,  s4) = parseLenMod s3
            pfx         = flags ++ width ++ prec
        in case s4 of
               (c:_) -> (pfx, lmod, c)
               []    -> (pfx, lmod, '\0')

    -- | LP64-correct length modifier for each typedef name that
    --   'transformLongs' can produce.
    correctLmod :: String -> String
    correctLmod "int32_t"   = ""    -- %d / %u / %x
    correctLmod "uint32_t"  = ""    -- %u / %d / %x
    correctLmod "ptrdiff_t" = "t"   -- %td / %tu
    correctLmod "size_t"    = "z"   -- %zu / %zd
    correctLmod "intptr_t"  = "l"   -- %ld  (intptr_t == long on LP64)
    correctLmod "uintptr_t" = "l"   -- %lu  (uintptr_t == unsigned long on LP64)
    correctLmod _           = ""    -- unknown typedef: strip non-standard lmod

    typedefName :: CDeclarationSpecifier NodeInfo -> Maybe String
    typedefName (CTypeSpec (CTypeDef (Ident n _ _) _)) = Just n
    typedefName _                                        = Nothing

    setFmtString :: CExpression NodeInfo -> String -> CExpression NodeInfo
    setFmtString (CConst (CStrConst (CString _ wide) ci)) s =
        CConst (CStrConst (CString s wide) ci)
    setFmtString e _ = e

    -- | Consume one format specifier starting just after the @%@ character.
    --   Returns @(rawSpec, remainder)@ where @rawSpec@ does NOT include @%@.
    consumeSpec :: String -> (String, String)
    consumeSpec s =
        let (flags, s1) = span (`elem` "-+ #0")      s
            (width, s2) = span (`elem` "0123456789") s1
            (prec,  s3) = case s2 of
                              '.':r -> let (p, r') = span (`elem` "0123456789") r
                                       in ('.' : p, r')
                              _     -> ("", s2)
            (lmod,  s4) = parseLenMod s3
        in case s4 of
               []    -> (flags ++ width ++ prec ++ lmod,        [])
               (c:r) -> (flags ++ width ++ prec ++ lmod ++ [c],  r)

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


