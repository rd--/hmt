-- | Ronald C. Read. \"Every one a winner or how to avoid isomorphism
-- search when cataloguing combinatorial configurations.\" /Annals of
-- Discrete Mathematics/ 2:107–20, 1978.
module Music.Theory.Z.Read_1978 where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Z as T {- hmt -}
import qualified Music.Theory.Z.SRO as T {- hmt -}

-- | Coding.
type Code = Int

-- | Bit array.
type Array = [Bool]

-- | Pretty printer for 'Array'.
array_pp :: Array -> String
array_pp = map intToDigit . map fromEnum

-- | Parse PP of 'Array'.
--
-- > parse_array "01001" == [False,True,False,False,True]
parse_array :: String -> Array
parse_array = map (toEnum . digitToInt)

-- | Generate 'Code' from 'Array', the coding is most to least significant.
--
-- > array_to_code (map toEnum [1,1,0,0,1,0,0,0,1,1,1,0,0]) == 6428
array_to_code :: Array -> Code
array_to_code a =
    let n = length a
        f e j = if e then 2 ^ (n - j - 1) else 0
    in sum (zipWith f a [0..])

-- | Inverse of 'array_to_code'.
--
-- > code_to_array 13 6428 == map toEnum [1,1,0,0,1,0,0,0,1,1,1,0,0]
code_to_array :: Int -> Code -> Array
code_to_array n c = map (testBit c) [n - 1, n - 2 .. 0]

-- | Array to set.
--
-- > array_to_set (map toEnum [1,1,0,0,1,0,0,0,1,1,1,0,0]) == [0,1,4,8,9,10]
-- > encode [0,1,4,8,9,10] == 1811
array_to_set :: Integral i => [Bool] -> [i]
array_to_set =
    let f (i,e) = if e then Just i else Nothing
    in mapMaybe f . zip [0..]

-- | Inverse of 'array_to_set', /z/ is the degree of the array.
set_to_array :: Integral i => i -> [i] -> Array
set_to_array z p = map (`elem` p) [0 .. z - 1]

-- | 'array_to_code' of 'set_to_array'.
--
-- > set_to_code 12 [0,2,3,5] == 2880
-- > map (set_to_code 12) (T.z_ti_related (flip mod 12) [0,2,3,5])
set_to_code :: Integral i => i -> [i] -> Code
set_to_code z = array_to_code . set_to_array z

-- | Logical complement.
array_complement :: Array -> Array
array_complement = map not

-- | The /prime/ form is the 'maximum' encoding.
--
-- > array_is_prime (set_to_array 12 [0,2,3,5]) == False
array_is_prime :: Array -> Bool
array_is_prime a =
    let c = array_to_code a
        p = array_to_set a
        n = length a
        z = flip mod n
        u = maximum (map (set_to_code n) (T.z_ti_related z p))
    in c == u

-- | The augmentation rule adds @1@ in each empty slot at end of array.
--
-- > map array_pp (array_augment (parse_array "01000")) == ["01100","01010","01001"]
array_augment :: Array -> [Array]
array_augment a =
    let (z,a') = break id (reverse a)
        a'' = reverse a'
        n = length z
        f k = map (== k) [0 .. n - 1]
        x = map f [0 .. n - 1]
    in map (a'' ++) x

-- | Enumerate first half of the set-classes under given /prime/ function.
-- The second half can be derived as the complement of the first.
--
-- > import Music.Theory.Z12.Forte_1973
-- > length scs == 224
-- > map (length . scs_n) [0..12] == [1,1,6,12,29,38,50,38,29,12,6,1,1]
--
-- > let z12 = map (fmap (map array_to_set)) (enumerate_half array_is_prime 12)
-- > map (length . snd) z12 == [1,1,6,12,29,38,50]
--
-- This can become slow, edit /z/ to find out.  It doesn't matter
-- about /n/.  This can be edited so that small /n/ would run quickly
-- even for large /z/.
--
-- > fmap (map array_to_set) (lookup 5 (enumerate_half array_is_prime 16))
enumerate_half :: (Array -> Bool) -> Int -> [(Int,[Array])]
enumerate_half pr n =
    let a0 = replicate n False
        f k a = if k >= n `div` 2
                then []
                else let r = filter pr (array_augment a)
                     in (k + 1,r) : concatMap (f (k + 1)) r
        jn l = case l of
                 (x,y):l' -> (x,concat (y : map snd l'))
                 _ -> error ""
        post_proc = map jn . T.group_on fst . sortOn fst
    in post_proc ((0,[a0]) : f 0 a0)

-- * Alternate (reverse) form.

-- | Encoder for 'encode_prime'.
--
-- > encode [0,1,3,6,8,9] == 843
encode :: Integral i => [i] -> Code
encode = sum . map (2 ^)

-- | Decoder for 'encode_prime'.
--
-- > decode 12 843 == [0,1,3,6,8,9]
decode :: Integral i => i -> Code -> [i]
decode z n =
    let f i = (i,testBit n (fromIntegral i))
    in map fst (filter snd (map f [0 .. z - 1]))

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
--
-- > encode_prime 12 [0,1,3,6,8,9] == [0,2,3,6,7,9]
-- > Music.Theory.Z12.Rahn_1980.rahn_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
encode_prime :: Integral i => T.Z i -> [i] -> [i]
encode_prime z s =
    let t = map (\x -> T.z_tn z x s) (T.z_univ z)
        c = t ++ map (T.z_invert z 0) t
    in decode (T.z_modulus z) (minimum (map encode c))
