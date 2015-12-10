-- | "System.IO" related functions.
module Music.Theory.IO where

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Text as T {- text -}
import qualified Data.Text.Encoding as T {- text -}
import qualified System.Directory as D {- directory -}

-- | 'T.decodeUtf8' of 'B.readFile'.
read_file_utf8_text :: FilePath -> IO T.Text
read_file_utf8_text = fmap T.decodeUtf8 . B.readFile

-- | Read (strictly) a UTF-8 encoded text file, implemented via "Data.Text".
read_file_utf8 :: FilePath -> IO String
read_file_utf8 = fmap T.unpack . read_file_utf8_text

-- | Read file, current locale.  Note: strictness, see System.IO.Strict.
read_file_utf8_or :: String -> FilePath -> IO String
read_file_utf8_or s f = do
  x <- D.doesFileExist f
  if x then read_file_utf8 f else return s
