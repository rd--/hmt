module Music.Theory.Z.TTO where

import Data.List {- base -}
import Data.Maybe {- base -}
import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import qualified Music.Theory.Parse as T
import qualified Music.Theory.Set.List as T
import Music.Theory.Z

-- | Twelve-tone operator, of the form TMI.
data TTO t = TTO {tto_T :: t,tto_M :: Bool,tto_I :: Bool}
             deriving (Eq,Show)

tto_identity :: Num t => TTO t
tto_identity = TTO 0 False False

-- | Pretty printer.
tto_pp :: Show t => TTO t -> String
tto_pp (TTO t m i) = concat ['T' : show t,if m then "M" else "",if i then "I" else ""]

p_tto :: Integral t => P.GenParser Char () (TTO t)
p_tto = do
  _ <- P.char 'T'
  t <- T.parse_int
  m <- T.is_char 'M'
  i <- T.is_char 'I'
  P.eof
  return (TTO t m i)

-- | Parser, transposition must be decimal.
--
-- > map (tto_pp . tto_parse) (words "T5 T3I T11M T9MI")
tto_parse :: Integral i => String -> TTO i
tto_parse = either (\e -> error ("tto_parse failed\n" ++ show e)) id . P.parse p_tto ""

-- | The set of all 'TTO', given 'Z' function.
--
-- > length (z_tto_univ mod12) == 48
-- > map tto_pp (z_tto_univ mod12)
z_tto_univ :: (Enum t, Integral t) => Z t -> [TTO t]
z_tto_univ z = [TTO t m i | m <- [False,True], i <- [False,True], t <- z_univ z]

-- | M is ordinarily 5, but can be specified here.
--
-- > map (z_tto_f 5 mod12 (tto_parse "T1M")) [0,1,2,3] == [1,6,11,4]
z_tto_f :: Integral t => t -> Z t -> TTO t -> (t -> t)
z_tto_f mn z (TTO t m i) =
    let i_f = if i then z_negate z else id
        m_f = if m then z_mul z mn else id
        t_f = if t > 0 then z_add z t else id
    in t_f . m_f . i_f

-- | 'sort' of 'map' 'z_tto_f'.
--
-- > z_tto_apply 5 mod12 (tto_parse "T1M") [0,1,2,3] == [1,4,6,11]
z_tto_apply :: Integral t => t -> Z t -> TTO t -> [t] -> [t]
z_tto_apply mn z o = sort . map (z_tto_f mn z o)

tto_apply :: Integral t => t -> TTO t -> [t] -> [t]
tto_apply mn = z_tto_apply mn id

-- | Find 'TTO' that that map /x/ to /y/ given /m/ and /z/.
--
-- > map tto_pp (z_tto_rel 5 mod12 [0,1,2,3] [6,4,1,11]) == ["T1M","T4MI"]
z_tto_rel :: (Eq t,Ord t,Enum t,Integral t) => t -> Z t -> [t] -> [t] -> [TTO t]
z_tto_rel m z x y =
    let q = T.set y
    in mapMaybe (\o -> if z_tto_apply m z o x == q then Just o else Nothing) (z_tto_univ z)

-- | 'nub' of 'sort' of 'map' /z/.
--
-- > map (z_pcset mod12) [[0,6],[6,12],[12,18]] == replicate 3 [0,6]
z_pcset :: Ord t => Z t -> [t] -> [t]
z_pcset z = nub . sort . map z
