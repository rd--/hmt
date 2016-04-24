-- | Serial (ordered) pitch-class operations on 'Z'.
module Music.Theory.Z.SRO where

import Data.List {- base -}
import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import Music.Theory.Parse
import Music.Theory.Z

-- | Serial operator,of the form rRTMI.
data SRO t = SRO {sro_r :: t
                 ,sro_R :: Bool
                 ,sro_T :: t
                 ,sro_M :: Bool
                 ,sro_I :: Bool}
             deriving (Eq,Show)

-- | Printer in 'rnRTnMI' form.
sro_pp :: (Num t,Eq t,Show t) => SRO t -> String
sro_pp (SRO rN r tN m i) =
    concat [if rN /= 0 then 'r' : show rN else ""
           ,if r then "R" else ""
           ,'T' : show tN
           ,if m then "M" else ""
           ,if i then "I" else ""]

-- | Parse a Morris format serial operator descriptor.
--
-- > rnrtnmi "r2RT3MI" == SRO 2 True 3 True True
rnrtnmi :: Integral i => String -> SRO i
rnrtnmi s =
  let p = do r <- rot
             r' <- is_char 'R'
             _ <- P.char 'T'
             t <- get_int
             m <- is_char 'M'
             i <- is_char 'I'
             P.eof
             return (SRO r r' t m i)
      rot = P.option 0 (P.char 'r' >> get_int)
  in either
         (\e -> error ("rnRTnMI parse failed\n" ++ show e))
         id
         (P.parse p "" s)

-- | Transpose /p/ by /n/.
--
-- > z_tn mod5 4 [0,1,4] == [4,0,3]
-- > z_tn mod12 4 [1,5,6] == [5,9,10]
z_tn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_tn z n = fmap (z_add z n)

-- | Invert /p/ about /n/.
--
-- > z_invert mod5 0 [0,1,4] == [0,4,1]
-- > z_invert mod12 6 [4,5,6] == [8,7,6]
-- > z_invert mod12 0 [0,1,3] == [0,11,9]
z_invert :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_invert z n = fmap (\p -> z_sub z n (z_sub z p  n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > z_tni mod5 1 [0,1,3] == [1,0,3]
-- > z_tni mod12 4 [1,5,6] == [3,11,10]
-- > (z_invert mod12 0 . z_tn mod12 4) [1,5,6] == [7,3,2]
z_tni :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_tni z n = z_tn z n . z_invert z 0

-- | Modulo multiplication.
--
-- > z_mn mod12 11 [0,1,4,9] == z_tni mod12 0 [0,1,4,9]
z_mn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_mn z n = fmap (z_mul z n)

-- | T-related sequences of /p/.
--
-- > length (z_t_related mod12 [0,3,6,9]) == 12
-- > z_t_related mod5 [0,2] == [[0,2],[1,3],[2,4],[3,0],[4,1]]
z_t_related :: (Integral i, Functor f) => Z i -> f i -> [f i]
z_t_related z p = fmap (\n -> z_tn z n p) (z_univ z)

-- | T\/I-related sequences of /p/.
--
-- > length (z_ti_related mod12 [0,1,3]) == 24
-- > length (z_ti_related mod12 [0,3,6,9]) == 24
-- > z_ti_related mod12 [0] == map return [0..11]
z_ti_related :: (Eq (f i), Integral i, Functor f) => Z i -> f i -> [f i]
z_ti_related z p = nub (z_t_related z p ++ z_t_related z (z_invert z 0 p))

-- | R\/T\/I-related sequences of /p/.
--
-- > length (z_rti_related mod12 [0,1,3]) == 48
-- > length (z_rti_related mod12 [0,3,6,9]) == 24
z_rti_related :: Integral i => Z i -> [i] -> [[i]]
z_rti_related z p = let q = z_ti_related z p in nub (q ++ map reverse q)

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > z_tn_to mod12 5 [0,1,3] == [5,6,8]
-- > map (z_tn_to mod12 0) [[0,1,3],[1,3,0],[3,0,1]]
z_tn_to :: Integral i => Z i -> i -> [i] -> [i]
z_tn_to z n p =
    case p of
      [] -> []
      x:xs -> n : z_tn z (z_sub z n x) xs

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (z_invert_ix mod12 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (z_invert_ix mod12 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
z_invert_ix :: Integral i => Z i -> Int -> [i] -> [i]
z_invert_ix z n p = z_invert z (p !! n) p

-- | The standard t-matrix of /p/.
--
-- > z_tmatrix mod12 [0,1,3] == [[0,1,3],[11,0,2],[9,10,0]]
z_tmatrix :: Integral i => Z i -> [i] -> [[i]]
z_tmatrix z p = map (\n -> z_tn z n p) (z_tn_to z 0 (z_invert_ix z 0 p))
