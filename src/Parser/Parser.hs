module Parser.Parser where

import Language.C
import qualified Data.ByteString.Char8 as BS

-- Pure: parse C source code from a ByteString
parseSource :: BS.ByteString -> Either ParseError CTranslUnit
parseSource src = parseC src (initPos "<unknown>")
