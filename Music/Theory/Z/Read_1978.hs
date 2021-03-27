-- | Ronald C. Read. \"Every one a winner or how to avoid isomorphism
-- search when cataloguing combinatorial configurations.\" /Annals of
-- Discrete Mathematics/ 2:107–20, 1978.
module Music.Theory.Z.Read_1978 where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Z as Z {- hmt -}
import qualified Music.Theory.Z.SRO as SRO {- hmt -}

-- | Coding.
type Code = Word64

-- | Number of bits at 'Code'.
code_len :: Num n => n
code_len = 64

-- | Bit array.
type Bit_Array = [Bool]

-- | Logical complement.
bit_array_complement :: Bit_Array -> Bit_Array
bit_array_complement = map not

-- | Pretty printer for 'Bit_Array'.
bit_array_pp :: Bit_Array -> String
bit_array_pp = map (intToDigit . fromEnum)

-- | Parse PP of 'Bit_Array'.
--
-- > bit_array_parse "01001" == [False,True,False,False,True]
bit_array_parse :: String -> Bit_Array
bit_array_parse = map (toEnum . digitToInt)

-- * MSB (BIG-ENDIAN)

-- | Generate 'Code' from 'Bit_Array', the coding is most to least significant.
--
-- > map (bit_array_to_code . bit_array_parse) (words "000 001 010 011 100 101 110 111") == [0..7]
-- > bit_array_to_code (bit_array_parse "1100100011100") == 6428
bit_array_to_code :: Bit_Array -> Code
bit_array_to_code a =
  let n = length a
      f e j = if e then 2 ^ (n - j - 1) else 0
  in if n > code_len
     then error "bit_array_to_code: > SZ"
     else sum (zipWith f a [0..])

-- | Inverse of 'bit_array_to_code'.
--
-- > code_to_bit_array 13 6428 == bit_array_parse "1100100011100"
code_to_bit_array :: Int -> Code -> Bit_Array
code_to_bit_array n c =
  if n > code_len
  then error "code_to_bit_array: > SZ"
  else map (testBit c) [n - 1, n - 2 .. 0]

-- | 'Bit_Array' to set.
--
-- > bit_array_to_set (bit_array_parse "1100100011100") == [0,1,4,8,9,10]
-- > set_to_code 13 [0,1,4,8,9,10] == 6428
bit_array_to_set :: Integral i => Bit_Array -> [i]
bit_array_to_set =
    let f (i,e) = if e then Just i else Nothing
    in mapMaybe f . zip [0..]

-- | Inverse of 'bit_array_to_set', /z/ is the degree of the array.
set_to_bit_array :: Integral i => i -> [i] -> Bit_Array
set_to_bit_array z p =
  if z > code_len
  then error "set_to_bit_array: > SZ"
  else map (`elem` p) [0 .. z - 1]

-- | 'bit_array_to_code' of 'set_to_bit_array'.
--
-- > set_to_code 12 [0,2,3,5] == 2880
-- > map (set_to_code 12) (SRO.z_sro_ti_related (flip mod 12) [0,2,3,5])
set_to_code :: Integral i => i -> [i] -> Code
set_to_code z = bit_array_to_code . set_to_bit_array z

-- | The /prime/ form is the 'maximum' encoding.
--
-- > bit_array_is_prime (set_to_bit_array 12 [0,2,3,5]) == False
bit_array_is_prime :: Bit_Array -> Bool
bit_array_is_prime a =
    let c = bit_array_to_code a
        p = bit_array_to_set a
        n = length a
        z = Z.Z n
        u = maximum (map (set_to_code n) (SRO.z_sro_ti_related z p))
    in c == u

-- | The augmentation rule adds @1@ in each empty slot at end of array.
--
-- > map bit_array_pp (bit_array_augment (bit_array_parse "01000")) == ["01100","01010","01001"]
bit_array_augment :: Bit_Array -> [Bit_Array]
bit_array_augment a =
    let (z,a') = break id (reverse a)
        a'' = reverse a'
        n = length z
        f k = map (== k) [0 .. n - 1]
        x = map f [0 .. n - 1]
    in map (a'' ++) x

-- | Enumerate first half of the set-classes under given /prime/ function.
--   The second half can be derived as the complement of the first.
--
-- > import Music.Theory.Z.Forte_1973
-- > length scs == 224
-- > map (length . scs_n) [0..12] == [1,1,6,12,29,38,50,38,29,12,6,1,1]
--
-- > let z12 = map (fmap (map bit_array_to_set)) (enumerate_half bit_array_is_prime 12)
-- > map (length . snd) z12 == [1,1,6,12,29,38,50]
--
-- This can become slow, edit /z/ to find out.  It doesn't matter
-- about /n/.  This can be edited so that small /n/ would run quickly
-- even for large /z/.
--
-- > fmap (map bit_array_to_set) (lookup 5 (enumerate_half bit_array_is_prime 16))
enumerate_half :: (Bit_Array -> Bool) -> Int -> [(Int,[Bit_Array])]
enumerate_half pr n =
    let a0 = replicate n False
        f k a = if k >= n `div` 2
                then []
                else let r = filter pr (bit_array_augment a)
                     in (k + 1,r) : concatMap (f (k + 1)) r
        jn l = case l of
                 (x,y):l' -> (x,concat (y : map snd l'))
                 _ -> error ""
        post_proc = map jn . List.group_on fst . sortOn fst
    in post_proc ((0,[a0]) : f 0 a0)

-- * LSB - LITTLE-ENDIAN

-- | If the size of the set is '>' 'code_len' then 'error', else 'id'.
set_coding_validate :: [t] -> [t]
set_coding_validate l = if length l <= code_len then l else error "set_coding_validate: SIZE"

-- | Encoder for 'encode_prime'.
--
-- > map set_encode [[0,1,3,7,8],[0,1,3,6,8,9]] == [395,843]
--
-- > map (set_to_code 12) [[0,1,3,7,8],[0,1,3,6,8,9]] == [3352,3372]
set_encode :: Integral i => [i] -> Code
set_encode = sum . map (2 ^) . set_coding_validate

-- | Decoder for 'encode_prime'.
--
-- > map (set_decode 12) [395,843] == [[0,1,3,7,8],[0,1,3,6,8,9]]
set_decode :: Integral i => Int -> Code -> [i]
set_decode z n =
    let f i = (fromIntegral i,testBit n i)
    in map fst (filter snd (map f [0 .. z - 1]))

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
--
-- > set_encode_prime Z.z12 [0,1,3,6,8,9] == [0,2,3,6,7,9]
-- > Music.Theory.Z.Rahn_1980.rahn_prime Z.z12 [0,1,3,6,8,9] == [0,2,3,6,7,9]
set_encode_prime :: Integral i => Z.Z i -> [i] -> [i]
set_encode_prime z s =
    let t = map (\x -> SRO.z_sro_tn z x s) (Z.z_univ z)
        c = t ++ map (SRO.z_sro_invert z 0) t
    in set_decode (fromIntegral (Z.z_modulus z)) (minimum (map set_encode c))
