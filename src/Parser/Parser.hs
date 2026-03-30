module Parser.Parser where

import Language.C
import qualified Data.ByteString.Char8 as BS

-- Parse a bytestring into C code
parseSource :: BS.ByteString -> Either ParseError CTranslUnit
parseSource src = parseC src (initPos "<unknown>")

parseSourceString :: String -> Either ParseError CTranslUnit
parseSourceString src = parseSource (BS.pack src)

-- | Read a file from disk and parse it, using the filename in position info.
parseSourceFile :: FilePath -> IO (Either ParseError CTranslUnit)
parseSourceFile path = do
    src <- BS.readFile path
    return $ parseC src (initPos path)