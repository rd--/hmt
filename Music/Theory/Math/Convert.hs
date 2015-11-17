-- | Specialised type conversions.
--
-- > map int_to_word8 [-1,256] == [255,0]
-- > map int_to_word8_maybe [-1,0,255,256] == [Nothing,Just 0,Just 255,Nothing]
module Music.Theory.Math.Convert where

import Data.Int {- base -}
import Data.Word {- base -}

-- | Type specialised 'fromIntegral'
word8_to_word16 :: Word8 -> Word16
word8_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_word32 :: Word8 -> Word32
word8_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_int16 :: Word8 -> Int16
word8_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_int32 :: Word8 -> Int32
word8_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_int :: Word8 -> Int
word8_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_integer :: Word8 -> Integer
word8_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_float :: Word8 -> Float
word8_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_double :: Word8 -> Double
word8_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_word8 :: Word16 -> Word8
word16_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_word32 :: Word16 -> Word32
word16_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_int16 :: Word16 -> Int16
word16_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_int32 :: Word16 -> Int32
word16_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_int :: Word16 -> Int
word16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_integer :: Word16 -> Integer
word16_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_float :: Word16 -> Float
word16_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
word16_to_double :: Word16 -> Double
word16_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_word8 :: Word32 -> Word8
word32_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_word16 :: Word32 -> Word16
word32_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int16 :: Word32 -> Int16
word32_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int32 :: Word32 -> Int32
word32_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_int :: Word32 -> Int
word32_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_integer :: Word32 -> Integer
word32_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_float :: Word32 -> Float
word32_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
word32_to_double :: Word32 -> Double
word32_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_word8 :: Int16 -> Word8
int16_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_word16 :: Int16 -> Word16
int16_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_word32 :: Int16 -> Word32
int16_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_int32 :: Int16 -> Int32
int16_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_int :: Int16 -> Int
int16_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_integer :: Int16 -> Integer
int16_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_float :: Int16 -> Float
int16_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
int16_to_double :: Int16 -> Double
int16_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_word8 :: Int32 -> Word8
int32_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_word16 :: Int32 -> Word16
int32_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_word32 :: Int32 -> Word32
int32_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_int16 :: Int32 -> Int16
int32_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_int :: Int32 -> Int
int32_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_integer :: Int32 -> Integer
int32_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_float :: Int32 -> Float
int32_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
int32_to_double :: Int32 -> Double
int32_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_word8 :: Int -> Word8
int_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_word16 :: Int -> Word16
int_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_word32 :: Int -> Word32
int_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int16 :: Int -> Int16
int_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_int32 :: Int -> Int32
int_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_integer :: Int -> Integer
int_to_integer = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_float :: Int -> Float
int_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
int_to_double :: Int -> Double
int_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_word8 :: Integer -> Word8
integer_to_word8 = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_word16 :: Integer -> Word16
integer_to_word16 = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_word32 :: Integer -> Word32
integer_to_word32 = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_int16 :: Integer -> Int16
integer_to_int16 = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_int32 :: Integer -> Int32
integer_to_int32 = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_int :: Integer -> Int
integer_to_int = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_float :: Integer -> Float
integer_to_float = fromIntegral

-- | Type specialised 'fromIntegral'
integer_to_double :: Integer -> Double
integer_to_double = fromIntegral

-- | Type specialised 'fromIntegral'
word8_to_word16_maybe :: Word8 -> Maybe Word16
word8_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word8_to_word32_maybe :: Word8 -> Maybe Word32
word8_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word8_to_int16_maybe :: Word8 -> Maybe Int16
word8_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word8_to_int32_maybe :: Word8 -> Maybe Int32
word8_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word8_to_int_maybe :: Word8 -> Maybe Int
word8_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word16_to_word8_maybe :: Word16 -> Maybe Word8
word16_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word16_to_word32_maybe :: Word16 -> Maybe Word32
word16_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word16_to_int16_maybe :: Word16 -> Maybe Int16
word16_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word16_to_int32_maybe :: Word16 -> Maybe Int32
word16_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word16_to_int_maybe :: Word16 -> Maybe Int
word16_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word32_to_word8_maybe :: Word32 -> Maybe Word8
word32_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word32_to_word16_maybe :: Word32 -> Maybe Word16
word32_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word32_to_int16_maybe :: Word32 -> Maybe Int16
word32_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word32_to_int32_maybe :: Word32 -> Maybe Int32
word32_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
word32_to_int_maybe :: Word32 -> Maybe Int
word32_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int16_to_word8_maybe :: Int16 -> Maybe Word8
int16_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int16_to_word16_maybe :: Int16 -> Maybe Word16
int16_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int16_to_word32_maybe :: Int16 -> Maybe Word32
int16_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int16_to_int32_maybe :: Int16 -> Maybe Int32
int16_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int16_to_int_maybe :: Int16 -> Maybe Int
int16_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int32_to_word8_maybe :: Int32 -> Maybe Word8
int32_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int32_to_word16_maybe :: Int32 -> Maybe Word16
int32_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int32_to_word32_maybe :: Int32 -> Maybe Word32
int32_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int32_to_int16_maybe :: Int32 -> Maybe Int16
int32_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int32_to_int_maybe :: Int32 -> Maybe Int
int32_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int_to_word8_maybe :: Int -> Maybe Word8
int_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int_to_word16_maybe :: Int -> Maybe Word16
int_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int_to_word32_maybe :: Int -> Maybe Word32
int_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int_to_int16_maybe :: Int -> Maybe Int16
int_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
int_to_int32_maybe :: Int -> Maybe Int32
int_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_word8_maybe :: Integer -> Maybe Word8
integer_to_word8_maybe n =
    if n < fromIntegral (minBound::Word8) ||
       n > fromIntegral (maxBound::Word8)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_word16_maybe :: Integer -> Maybe Word16
integer_to_word16_maybe n =
    if n < fromIntegral (minBound::Word16) ||
       n > fromIntegral (maxBound::Word16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_word32_maybe :: Integer -> Maybe Word32
integer_to_word32_maybe n =
    if n < fromIntegral (minBound::Word32) ||
       n > fromIntegral (maxBound::Word32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_int16_maybe :: Integer -> Maybe Int16
integer_to_int16_maybe n =
    if n < fromIntegral (minBound::Int16) ||
       n > fromIntegral (maxBound::Int16)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_int32_maybe :: Integer -> Maybe Int32
integer_to_int32_maybe n =
    if n < fromIntegral (minBound::Int32) ||
       n > fromIntegral (maxBound::Int32)
    then Nothing
    else Just (fromIntegral n)

-- | Type specialised 'fromIntegral'
integer_to_int_maybe :: Integer -> Maybe Int
integer_to_int_maybe n =
    if n < fromIntegral (minBound::Int) ||
       n > fromIntegral (maxBound::Int)
    then Nothing
    else Just (fromIntegral n)
