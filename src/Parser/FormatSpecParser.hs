-- | Shared parser for C printf-style length modifiers.
--
--   The same parsing logic was previously duplicated across
--   'Analysis.FormatStrings', 'Linter.FormatStrings', and
--   'Transformer.FormatFix'.  Centralising it here means a bug
--   fix or extension only needs to be applied once.
module Parser.FormatSpecParser
    ( parseLenMod
    ) where

-- | Consume a C printf length modifier from the start of a string.
--   Returns @(modifier, remainder)@.  @modifier@ is one of
--   @""@, @"h"@, @"hh"@, @"l"@, @"ll"@, @"j"@, @"z"@, @"t"@, @"L"@, @"q"@.
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
