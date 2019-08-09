-- | Byte functions.
module Music.Theory.Byte where

import Data.Char {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Numeric {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.List.Split as Split {- split -}
import qualified Safe {- safe -}

import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

{-
import Data.Int {- base -}
import qualified Data.ByteString.Lazy as L {- bytestring -}

-- * LBS

-- | Section function for 'L.ByteString', ie. from (n,m).
--
-- > lbs_slice 4 5 (L.pack [1..10]) == L.pack [5,6,7,8,9]
lbs_slice :: Int64 -> Int64 -> L.ByteString -> L.ByteString
lbs_slice n m = L.take m . L.drop n

-- | Variant of slice with start and end indices (zero-indexed).
--
-- > lbs_section 4 8 (L.pack [1..]) == L.pack [5,6,7,8,9]
lbs_section :: Int64 -> Int64 -> L.ByteString -> L.ByteString
lbs_section l r = L.take (r - l + 1) . L.drop l
-}

-- * Enumerations & Char

-- | 'toEnum' of 'T.word8_to_int'
word8_to_enum :: Enum e => Word8 -> e
word8_to_enum = toEnum . T.word8_to_int

-- | 'T.int_to_word8_maybe' of 'fromEnum'
enum_to_word8 :: Enum e => e -> Maybe Word8
enum_to_word8 = T.int_to_word8_maybe . fromEnum

-- | Type-specialised 'word8_to_enum'
--
-- > map word8_to_char [60,62] == "<>"
word8_to_char :: Word8 -> Char
word8_to_char = word8_to_enum

-- | 'T.int_to_word8' of 'fromEnum'
char_to_word8 :: Char -> Word8
char_to_word8 = T.int_to_word8 . fromEnum

-- | 'T.int_to_word8' of 'digitToInt'
digit_to_word8 :: Char -> Word8
digit_to_word8 = T.int_to_word8 . digitToInt

-- | 'intToDigit' of 'T.word8_to_int'
word8_to_digit :: Word8 -> Char
word8_to_digit = intToDigit . T.word8_to_int

-- * Indexing

-- | 'Safe.at' of 'T.word8_to_int'
word8_at :: [t] -> Word8 -> t
word8_at l = Safe.at l . T.word8_to_int

-- * Text

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

-- | 'byte_hex_pp_err' either plain (ws = False) or with spaces (ws = True).
--   Plain is the same format written by xxd -p and read by xxd -r -p.
--
-- > byte_seq_hex_pp True [0x0F,0xF0] == "0F F0"
byte_seq_hex_pp :: (Integral i, Show i) => Bool -> [i] -> String
byte_seq_hex_pp ws = (if ws then unwords else concat) . map byte_hex_pp_err

-- | Read two character hexadecimal string.
--
-- > mapMaybe read_hex_byte (Split.chunksOf 2 "0FF0F") == [0x0F,0xF0]
read_hex_byte :: (Eq t,Num t) => String -> Maybe t
read_hex_byte s =
    case s of
      [_,_] -> T.reads_to_read_precise readHex s
      _ -> Nothing

-- | Erroring variant.
read_hex_byte_err :: (Eq t,Num t) => String -> t
read_hex_byte_err = fromMaybe (error "read_hex_byte") . read_hex_byte

-- | Sequence of 'read_hex_byte_err'
--
-- > read_hex_byte_seq "000FF0FF" == [0x00,0x0F,0xF0,0xFF]
read_hex_byte_seq :: (Eq t,Num t) => String -> [t]
read_hex_byte_seq = map read_hex_byte_err . Split.chunksOf 2

-- | Variant that filters white space.
--
-- > read_hex_byte_seq_ws "00 0F F0 FF" == [0x00,0x0F,0xF0,0xFF]
read_hex_byte_seq_ws :: (Eq t,Num t) => String -> [t]
read_hex_byte_seq_ws = read_hex_byte_seq . filter (not . isSpace)

-- * IO

-- | Load binary 'U8' sequence from file.
load_byte_seq :: Integral i => FilePath -> IO [i]
load_byte_seq = fmap (map fromIntegral . B.unpack) . B.readFile

-- | Store binary 'U8' sequence to file.
store_byte_seq :: Integral i => FilePath -> [i] -> IO ()
store_byte_seq fn = B.writeFile fn . B.pack . map fromIntegral

-- | Load hexadecimal text 'U8' sequences from file.
load_hex_byte_seq :: Integral i => FilePath -> IO [[i]]
load_hex_byte_seq = fmap (map read_hex_byte_seq . lines) . readFile

-- | Store 'U8' sequences as hexadecimal text, one sequence per line.
store_hex_byte_seq :: (Integral i,Show i) => FilePath -> [[i]] -> IO ()
store_hex_byte_seq fn = writeFile fn . unlines . map (byte_seq_hex_pp False)

{-

import qualified Data.ByteString.Base64 as Base64 {- base64-bytestring -}
let fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/ROM1A.syx"
b <- load_byte_seq fn :: IO [Word8]
let e = B.unpack (Base64.encode (B.pack b))
let r = B.unpack (Base64.decodeLenient (B.pack e))
(length b,length e,length r,b == r) == (4104,5472,4104,True)
map word8_to_char e

-}
