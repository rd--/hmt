-- | "System.IO" related functions.
module Music.Theory.IO where

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.Text as T {- text -}
import qualified Data.Text.Encoding as T {- text -}
import qualified Data.Text.IO as T {- text -}
import qualified System.Directory as D {- directory -}

-- | 'T.decodeUtf8' of 'B.readFile'.
read_file_utf8_text :: FilePath -> IO T.Text
read_file_utf8_text = fmap T.decodeUtf8 . B.readFile

-- | Read (strictly) a UTF-8 encoded text file, implemented via "Data.Text".
read_file_utf8 :: FilePath -> IO String
read_file_utf8 = fmap T.unpack . read_file_utf8_text

-- | 'read_file_utf8', or a default value if the file doesn't exist.
read_file_utf8_or :: String -> FilePath -> IO String
read_file_utf8_or def f = do
  x <- D.doesFileExist f
  if x then read_file_utf8 f else return def

-- | Write UTF8 string as file, via "Data.Text".
write_file_utf8 :: FilePath -> String -> IO ()
write_file_utf8 fn = B.writeFile fn . T.encodeUtf8 . T.pack

-- | 'readFile' variant using 'T.Text' for @ISO 8859-1@ (Latin 1) encoding.
read_file_iso_8859_1 :: FilePath -> IO String
read_file_iso_8859_1 = fmap (T.unpack . T.decodeLatin1) . B.readFile

-- | 'readFile' variant using 'T.Text' for local encoding.
read_file_locale :: FilePath -> IO String
read_file_locale = fmap T.unpack . T.readFile
