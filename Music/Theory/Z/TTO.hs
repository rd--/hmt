module Music.Theory.Z.TTO where

import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import Music.Theory.Parse
import Music.Theory.Z

-- | Twelve-tone operator,of the form TMI.
data TTO t = TTO {tto_T :: t,tto_M :: Bool,tto_I :: Bool}
             deriving (Eq,Show)

-- | Pretty printer.
tto_pp :: Show t => TTO t -> String
tto_pp o = concat ["T",show (tto_T o),if tto_M o then "M" else "",if tto_I o then "I" else ""]

-- | Parser, transposition must be decimal.
--
-- > map (tto_pp . tto_parse) (words "T5 T3I T11M T9MI")
tto_parse :: Integral i => String -> TTO i
tto_parse s =
  let p = do _ <- P.char 'T'
             t <- get_int
             m <- is_char 'M'
             i <- is_char 'I'
             P.eof
             return (TTO t m i)
  in either (\e -> error ("tto_parse failed\n" ++ show e)) id (P.parse p "" s)

-- | The set of all 'TTO', given 'Z' function.
--
-- > length (z_tto_univ mod12) == 48
z_tto_univ :: (Enum t, Integral t) => Z t -> [TTO t]
z_tto_univ z = [TTO t m i | t <- z_univ z, m <- [False,True], i <- [False,True]]

-- | M is ordinarily 5, but can be specified here.
--
-- > z_tto_apply 5 mod12 (tto_parse "T1M") [0,1,2,3] == [1,6,11,4]
z_tto_apply :: Integral t => t -> Z t -> TTO t -> [t] -> [t]
z_tto_apply mn z (TTO t m i) =
    let i_f = if i then map (z_negate z) else id
        m_f = if m then map (z_mul z mn) else id
        t_f = if t > 0 then map (z_add z t) else id
    in t_f . m_f . i_f
