-- | Bits functions.
module Music.Theory.Bits where

import Data.Bits {- base -}

bit_pp :: Bool -> Char
bit_pp b = if b then '1' else '0'

bits_pp :: [Bool] -> String
bits_pp = map bit_pp

-- | Generate /n/ place bit sequence for /x/.
gen_bitseq :: FiniteBits b => Int -> b -> [Bool]
gen_bitseq n x =
    if finiteBitSize x < n
    then error "gen_bitseq"
    else map (testBit x) (reverse [0 .. n - 1])

-- | 'bits_pp' of 'gen_bitseq'.
--
-- > :set -XBinaryLiterals
-- > 0xF0 == 0b11110000
-- > gen_bitseq_pp 8 (0xF0::Int) == "11110000"
gen_bitseq_pp :: FiniteBits b => Int -> b -> String
gen_bitseq_pp n = bits_pp . gen_bitseq n
