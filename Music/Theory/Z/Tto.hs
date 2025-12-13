-- | Generalised twelve-tone operations on un-ordered pitch-class sets with arbitrary Z.
module Music.Theory.Z.Tto where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Text.Parsec as P {- parsec -}

import qualified Music.Theory.Math.Z as Z {- hmt-base -}
import qualified Music.Theory.Parse as Parse {- hmt-base -}

-- * Tto

-- | Twelve-tone operator, of the form TMI.
data Tto t = Tto {tto_T :: t, tto_M :: t, tto_I :: Bool}
  deriving (Eq, Show)

-- | T0
tto_identity :: Num t => Tto t
tto_identity = Tto 0 1 False

-- | Pretty printer.  It is an error here is M is not 1 or 5.
tto_pp :: (Show t, Num t, Eq t) => Tto t -> String
tto_pp (Tto t m i) =
  concat
    [ 'T' : show t
    , if m == 1 then "" else if m == 5 then "M" else error "tto_pp: M?"
    , if i then "I" else ""
    ]

-- | Parser for Tto, requires value for M (ordinarily 5 for 12-tone Tto).
p_tto :: Integral t => t -> Parse.P (Tto t)
p_tto m_mul = do
  _ <- P.char 'T'
  t <- Parse.parse_int
  m <- Parse.is_char 'M'
  i <- Parse.is_char 'I'
  P.eof
  return (Tto t (if m then m_mul else 1) i)

{- | Parser, transposition must be decimal.

>>> map (tto_pp . tto_parse 5) (words "T5 T3I T11M T9MI")
["T5","T3I","T11M","T9MI"]
-}
tto_parse :: Integral i => i -> String -> Tto i
tto_parse m = either (\e -> error ("tto_parse failed\n" ++ show e)) id . P.parse (p_tto m) ""

-- | Set M at Tto.
tto_M_set :: Integral t => t -> Tto t -> Tto t
tto_M_set m (Tto t _ i) = Tto t m i

-- * Z

{- | The set of all 'Tto', given 'Z'.

>>> length (z_tto_univ 5 Z.z12)
48

>>> map tto_pp (z_tto_univ 5 Z.z12)
["T0","T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T0I","T1I","T2I","T3I","T4I","T5I","T6I","T7I","T8I","T9I","T10I","T11I","T0M","T1M","T2M","T3M","T4M","T5M","T6M","T7M","T8M","T9M","T10M","T11M","T0MI","T1MI","T2MI","T3MI","T4MI","T5MI","T6MI","T7MI","T8MI","T9MI","T10MI","T11MI"]
-}
z_tto_univ :: Integral t => t -> Z.Z t -> [Tto t]
z_tto_univ m_mul z = [Tto t m i | m <- [1, m_mul], i <- [False, True], t <- Z.z_univ z]

{- | Apply Tto to pitch-class.

>>> map (z_tto_f Z.z12 (tto_parse 5 "T1M")) [0,1,2,3]
[1,6,11,4]
-}
z_tto_f :: Integral t => Z.Z t -> Tto t -> (t -> t)
z_tto_f z (Tto t m i) =
  let i_f = if i then Z.z_negate z else id
      m_f = if m == 1 then id else Z.z_mul z m
      t_f = if t > 0 then Z.z_add z t else id
  in t_f . m_f . i_f

{- | 'nub' of 'sort' of 'z_tto_f'.  (nub because M may be 0).

>>> z_tto_apply Z.z12 (tto_parse 5 "T1M") [0,1,2,3]
[1,4,6,11]
-}
z_tto_apply :: Integral t => Z.Z t -> Tto t -> [t] -> [t]
z_tto_apply z o = nub . sort . map (z_tto_f z o)

{- | Find 'Tto's that map pc-set /x/ to pc-set /y/ given /m/ and /z/.

>>> map tto_pp (z_tto_rel 5 Z.z12 [0,1,2,3] [1,4,6,11])
["T1M","T4MI"]
-}
z_tto_rel :: (Ord t, Integral t) => t -> Z.Z t -> [t] -> [t] -> [Tto t]
z_tto_rel m z x y =
  let f o = if z_tto_apply z o x == y then Just o else Nothing
  in mapMaybe f (z_tto_univ m z)

-- * Plain

{- | 'nub' of 'sort' of 'Z.z_mod' of /z/.

>>> z_pcset Z.z12 [1,13]
[1]

>>> map (z_pcset Z.z12) [[0,6],[6,12],[12,18]]
[[0,6],[0,6],[0,6]]
-}
z_pcset :: (Integral t, Ord t) => Z.Z t -> [t] -> [t]
z_pcset z = nub . sort . map (Z.z_mod z)

{- | Transpose by n.

>>> z_tto_tn Z.z12 4 [1,5,6]
[5,9,10]

>>> z_tto_tn Z.z12 4 [0,4,8]
[0,4,8]
-}
z_tto_tn :: Integral i => Z.Z i -> i -> [i] -> [i]
z_tto_tn z n = sort . map (Z.z_add z n)

{- | Invert about n.

>>> z_tto_invert Z.z12 6 [4,5,6]
[6,7,8]

>>> z_tto_invert Z.z12 0 [0,1,3]
[0,9,11]
-}
z_tto_invert :: Integral i => Z.Z i -> i -> [i] -> [i]
z_tto_invert z n = sort . map (\p -> Z.z_sub z n (Z.z_sub z p n))

{- | Composition of 'z_tto_invert' about @0@ and 'z_tto_tn'.

>>> z_tto_tni Z.z12 4 [1,5,6]
[3,10,11]

>>> (z_tto_invert Z.z12 0 . z_tto_tn Z.z12 4) [1,5,6]
[2,3,7]
-}
z_tto_tni :: Integral i => Z.Z i -> i -> [i] -> [i]
z_tto_tni z n = z_tto_tn z n . z_tto_invert z 0

{- | Modulo-z multiplication

>>> z_tto_mn Z.z12 11 [0,1,4,9] == z_tto_invert Z.z12 0 [0,1,4,9]
True
-}
z_tto_mn :: Integral i => Z.Z i -> i -> [i] -> [i]
z_tto_mn z n = sort . map (Z.z_mul z n)

{- | M5, ie. 'mn' @5@.

>>> z_tto_m5 Z.z12 [0,1,3]
[0,3,5]
-}
z_tto_m5 :: Integral i => Z.Z i -> [i] -> [i]
z_tto_m5 z = z_tto_mn z 5

-- * Sequence

-- | T-related sets of /p/.
z_tto_t_related_seq :: Integral i => Z.Z i -> [i] -> [[i]]
z_tto_t_related_seq z p = map (\q -> z_tto_tn z q p) [0 .. 11]

{- | Unique elements of 'z_tto_t_related_seq'.

>>> length (z_tto_t_related Z.z12 [0,1,3])
12

>>> z_tto_t_related Z.z12 [0,3,6,9]
[[0,3,6,9],[1,4,7,10],[2,5,8,11]]
-}
z_tto_t_related :: Integral i => Z.Z i -> [i] -> [[i]]
z_tto_t_related z = nub . z_tto_t_related_seq z

-- | T\/I-related set of /p/.
z_tto_ti_related_seq :: Integral i => Z.Z i -> [i] -> [[i]]
z_tto_ti_related_seq z p = z_tto_t_related z p ++ z_tto_t_related z (z_tto_invert z 0 p)

{- | Unique elements of 'z_tto_ti_related_seq'.

>>> length (z_tto_ti_related Z.z12 [0,1,3])
24

>>> z_tto_ti_related Z.z12 [0,3,6,9]
[[0,3,6,9],[1,4,7,10],[2,5,8,11]]
-}
z_tto_ti_related :: Integral i => Z.Z i -> [i] -> [[i]]
z_tto_ti_related z = nub . z_tto_ti_related_seq z
