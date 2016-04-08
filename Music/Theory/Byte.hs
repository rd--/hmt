-- | Byte functions.
module Music.Theory.Byte where

import qualified Data.ByteString as B {- bytestring -}
import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Numeric {- base -}

import qualified Music.Theory.Read as T {- hmt -}

-- | Given /n/ in (0,255) make two character hex string.
--
-- > mapMaybe byte_hex_pp [0x0F,0xF0,0xF0F] == ["0F","F0"]
byte_hex_pp :: (Integral i, Show i) => i -> Maybe String
byte_hex_pp n =
    case showHex n "" of
      [c] -> Just ['0',toUpper c]
      [c,d] -> Just (map toUpper [c,d])
      _ -> Nothing

-- | Erroring variant.
byte_hex_pp_err :: (Integral i, Show i) => i -> String
byte_hex_pp_err = fromMaybe (error "byte_hex_pp") . byte_hex_pp

-- | 'unwords' of 'map' of 'byte_hex_pp_err'.
--
-- > byte_seq_hex_pp [0x0F,0xF0] == "0F F0"
byte_seq_hex_pp :: (Integral i, Show i) => [i] -> String
byte_seq_hex_pp = unwords . map byte_hex_pp_err

-- | Read two character hexadecimal string.
read_hex_byte :: (Eq t,Num t) => String -> t
read_hex_byte s =
    case s of
      [_,_] -> T.reads_to_read_precise_err "readHex" readHex s
      _ -> error "read_hex_byte"

read_hex_byte_seq :: (Eq t,Num t) => String -> [t]
read_hex_byte_seq = map read_hex_byte . words

-- | Load binary 'U8' sequence from file.
load_byte_seq :: Integral i => FilePath -> IO [i]
load_byte_seq = fmap (map fromIntegral . B.unpack) . B.readFile

store_byte_seq :: Integral i => FilePath -> [i] -> IO ()
store_byte_seq fn = B.writeFile fn . B.pack . map fromIntegral

-- | Load hexadecimal text 'U8' sequence from file.
load_hex_byte_seq :: Integral i => FilePath -> IO [i]
load_hex_byte_seq = fmap (map read_hex_byte . words) . readFile

-- | Store 'U8' sequence as hexadecimal text, 16 words per line.
store_hex_byte_seq :: (Integral i,Show i) => FilePath -> [i] -> IO ()
store_hex_byte_seq fn = writeFile fn . unlines . map unwords . chunksOf 16 . map byte_hex_pp_err
