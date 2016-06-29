module Music.Theory.Random.I_Ching where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import System.Random {- random -}

import Music.Theory.Tuple {- hmt -}

{-| (sum={6,7,8,9},
     (yarrow probablity={1,3,5,7}/16,
      three-coin probablity={2,6}/16,
      name,signification,symbol))
-}
i_ching_chart :: (Num n,Fractional r) => [(n,(r,r,String,String,String))]
i_ching_chart =
    [(6,(1/16,2/16,"old yin","yin changing into yang","--- x ---"))
    ,(8,(7/16,6/16,"young yin","yin unchanging","---   ---"))
    ,(9,(3/16,2/16,"old yang","yang changing into yin","--- o ---"))
    ,(7,(5/16,6/16,"young yang","yang unchanging","---------"))]

-- | Nine character ASCII string for line, given sum.
--
-- > mapMaybe sum_ascii_pp [6..9]
sum_ascii_pp :: (Eq n, Num n) => n -> Maybe String
sum_ascii_pp n = fmap p5_fifth (lookup n i_ching_chart)

-- | 'fromMaybe' of 'sum_ascii_pp'
sum_ascii_pp_err :: (Eq n, Num n) => n -> String
sum_ascii_pp_err = fromMaybe (error "sum_ascii_pp") . sum_ascii_pp

-- | Is line (ie. sum) moving (ie. 6 or 9).
sum_is_moving :: (Eq n, Num n) => n -> Bool
sum_is_moving n = n `elem` [6,9]

-- | Old yin (6) becomes yang (7), and old yang (9) becomes yin (8).
sum_complement :: (Eq n, Num n) => n -> Maybe n
sum_complement n =
    case n of
      6 -> Just 7
      9 -> Just 8
      _ -> Nothing

-- | Hexagrams are drawn upwards.
hexagram_pp :: (Eq n, Num n) => [n] -> String
hexagram_pp = unlines . reverse . map sum_ascii_pp_err

{- | Sequence of sum values assigned to ascending four bit numbers.

> import  Music.Theory.Bits {- hmt -}
> zip (map (gen_bitseq_pp 4) [0::Int .. 15]) (map sum_ascii_pp_err four_coin_sequence)

-}
four_coin_sequence :: Num n => [n]
four_coin_sequence =
    [6,9,9,9
    ,7,7,7,7
    ,7,8,8,8
    ,8,8,8,8]

-- | Generate hexagram (ie. sequence of six lines given by sum) using 'four_coin_sequence'.
--
-- > four_coin_gen_hexagram >>= putStrLn . hexagram_pp
four_coin_gen_hexagram :: Num n => IO [n]
four_coin_gen_hexagram = fmap (map (four_coin_sequence !!)) (replicateM 6 (randomRIO (0,15)))

-- | 'any' of 'sum_is_moving'.
hexagram_has_complement :: (Num n,Eq n) => [n] -> Bool
hexagram_has_complement = any sum_is_moving

-- | If 'hexagram_has_complement' then derive it.
--
-- > h <- four_coin_gen_hexagram
-- > putStrLn (hexagram_pp h)
-- > maybe (return ()) (putStrLn . hexagram_pp) (hexagram_complement h)
hexagram_complement :: (Num n,Eq n) => [n] -> Maybe [n]
hexagram_complement h =
    let f n = fromMaybe n (sum_complement n)
    in if hexagram_has_complement h then Just (map f h) else Nothing
