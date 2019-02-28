-- | Byte functions.
module Music.Theory.Byte where

import Data.Char {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Numeric {- base -}

import qualified Data.ByteString as B {- bytestring -}
import Data.List.Split {- split -}
import qualified Safe {- safe -}

import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

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

-- | 'unwords' of 'byte_hex_pp_err'.
--
-- > byte_seq_hex_pp [0x0F,0xF0] == "0F F0"
byte_seq_hex_pp :: (Integral i, Show i) => [i] -> String
byte_seq_hex_pp = unwords . map byte_hex_pp_err

-- | Read two character hexadecimal string.
--
-- > mapMaybe read_hex_byte (words "0F F0 F") == [0x0F,0xF0]
read_hex_byte :: (Eq t,Num t) => String -> Maybe t
read_hex_byte s =
    case s of
      [_,_] -> T.reads_to_read_precise readHex s
      _ -> Nothing

-- | Erroring variant.
read_hex_byte_err :: (Eq t,Num t) => String -> t
read_hex_byte_err = fromMaybe (error "read_hex_byte") . read_hex_byte

-- | 'read_hex_byte_err' of 'words'
read_hex_byte_seq :: (Eq t,Num t) => String -> [t]
read_hex_byte_seq = map read_hex_byte_err . words

-- * IO

-- | Load binary 'U8' sequence from file.
load_byte_seq :: Integral i => FilePath -> IO [i]
load_byte_seq = fmap (map fromIntegral . B.unpack) . B.readFile

-- | Store binary 'U8' sequence to file.
store_byte_seq :: Integral i => FilePath -> [i] -> IO ()
store_byte_seq fn = B.writeFile fn . B.pack . map fromIntegral

-- | Load hexadecimal text 'U8' sequence from file.
load_hex_byte_seq :: Integral i => FilePath -> IO [i]
load_hex_byte_seq = fmap (map read_hex_byte_err . words) . readFile

-- | Store 'U8' sequence as hexadecimal text, /k/ words per line.
store_hex_byte_seq :: (Integral i,Show i) => Int -> FilePath -> [i] -> IO ()
store_hex_byte_seq k fn = writeFile fn . unlines . map unwords . chunksOf k . map byte_hex_pp_err
