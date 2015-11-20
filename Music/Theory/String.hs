-- | String functions.
module Music.Theory.String where

import Data.Char {- base -}
import qualified Data.ByteString as B {- text -}
import qualified Data.Text as T {- text -}
import qualified Data.Text.Encoding as T {- text -}
import qualified Data.Text.IO as T {- text -}

-- | 'readFile' variant using 'Text' for local encoding.
read_file_locale :: FilePath -> IO String
read_file_locale fn = do
  t <- T.readFile fn
  return (T.unpack t)

-- | 'readFile' variant using 'Text' for @ISO 8859-1@ (Latin 1) encoding.
read_file_iso_8859_1 :: FilePath -> IO String
read_file_iso_8859_1 fn = do
  b <- B.readFile fn
  return (T.unpack (T.decodeLatin1 b))

-- | Remove @\r@.
filter_cr :: String -> String
filter_cr = filter (not . (==) '\r')

-- > delete_trailing_whitespace "   str   " == "   str"
delete_trailing_whitespace :: String -> String
delete_trailing_whitespace = reverse . dropWhile isSpace . reverse
