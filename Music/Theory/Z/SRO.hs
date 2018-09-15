-- | Serial (ordered) pitch-class operations on 'Z'.
module Music.Theory.Z.SRO where

import Data.List {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as P {- parsec -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Parse as T

import Music.Theory.Z

-- | Serial operator,of the form rRTMI.
data SRO t = SRO {sro_r :: Int
                 ,sro_R :: Bool
                 ,sro_T :: t
                 ,sro_M :: Bool
                 ,sro_I :: Bool}
             deriving (Eq,Show)

-- | Printer in 'rnRTnMI' form.
sro_pp :: Show t => SRO t -> String
sro_pp (SRO rN r tN m i) =
    concat [if rN /= 0 then 'r' : show rN else ""
           ,if r then "R" else ""
           ,'T' : show tN
           ,if m then "M" else ""
           ,if i then "I" else ""]

p_sro :: Integral t => P.GenParser Char () (SRO t)
p_sro = do
  let rot = P.option 0 (P.char 'r' >> T.parse_int)
  r <- rot
  r' <- T.is_char 'R'
  _ <- P.char 'T'
  t <- T.parse_int
  m <- T.is_char 'M'
  i <- T.is_char 'I'
  P.eof
  return (SRO r r' t m i)

-- | Parse a Morris format serial operator descriptor.
--
-- > sro_parse "r2RT3MI" == SRO 2 True 3 True True
sro_parse :: Integral i => String -> SRO i
sro_parse =
    either (\e -> error ("sro_parse failed\n" ++ show e)) id .
    P.parse p_sro ""

-- | The total set of serial operations.
--
-- > let u = z_sro_univ 3 mod12
-- > zip (map sro_pp u) (map (\o -> z_sro_apply 5 mod12 o [0,1,3]) u)
z_sro_univ :: Integral i => Int -> Z i -> [SRO i]
z_sro_univ n_rot z =
    [SRO r r' t m i |
     r <- [0 .. n_rot - 1],
     r' <- [False,True],
     t <- z_univ z,
     m <- [False,True],
     i <- [False,True]]

-- | The set of transposition 'SRO's.
z_sro_Tn :: Integral i => Z i -> [SRO i]
z_sro_Tn z = [SRO 0 False n False False | n <- z_univ z]

-- | The set of transposition and inversion 'SRO's.
z_sro_TnI :: Integral i => Z i -> [SRO i]
z_sro_TnI z =
    [SRO 0 False n False i |
     n <- z_univ z,
     i <- [False,True]]

-- | The set of retrograde and transposition and inversion 'SRO's.
z_sro_RTnI :: Integral i => Z i -> [SRO i]
z_sro_RTnI z =
    [SRO 0 r n False i |
     r <- [True,False],
     n <- z_univ z,
     i <- [False,True]]

-- | The set of transposition, @M5@ and inversion 'SRO's.
z_sro_TnMI :: Integral i => Z i -> [SRO i]
z_sro_TnMI z =
    [SRO 0 False n m i |
     n <- z_univ z,
     m <- [True,False],
     i <- [True,False]]

-- | The set of retrograde,transposition,@M5@ and inversion 'SRO's.
z_sro_RTnMI :: Integral i => Z i -> [SRO i]
z_sro_RTnMI z =
    [SRO 0 r n m i |
     r <- [True,False],
     n <- z_univ z,
     m <- [True,False],
     i <- [True,False]]

-- * Serial operations

-- | Apply SRO.  M is ordinarily 5, but can be specified here.
--
-- > z_sro_apply 5 mod12 (SRO 1 True 1 True False) [0,1,2,3] == [11,6,1,4]
-- > z_sro_apply 5 mod12 (SRO 1 False 4 True True) [0,1,2,3] == [11,6,1,4]
z_sro_apply :: Integral i => i -> Z i -> SRO i -> [i] -> [i]
z_sro_apply mn z (SRO r r' t m i) x =
    let x1 = if i then z_sro_invert z 0 x else x
        x2 = if m then z_sro_mn z mn x1 else x1
        x3 = z_sro_tn z t x2
        x4 = if r' then reverse x3 else x3
    in T.rotate_left r x4

-- | Transpose /p/ by /n/.
--
-- > z_sro_tn mod5 4 [0,1,4] == [4,0,3]
-- > z_sro_tn mod12 4 [1,5,6] == [5,9,10]
z_sro_tn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_tn z n = fmap (z_add z n)

-- | Invert /p/ about /n/.
--
-- > z_sro_invert mod5 0 [0,1,4] == [0,4,1]
-- > z_sro_invert mod12 6 [4,5,6] == [8,7,6]
-- > z_sro_invert mod12 0 [0,1,3] == [0,11,9]
--
-- > import Data.Word {- base -}
-- > z_sro_invert mod12 (0::Word8) [1,4,8]
z_sro_invert :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_invert z n = fmap (\p -> z_sub z n (z_sub z p  n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > z_sro_tni mod5 1 [0,1,3] == [1,0,3]
-- > z_sro_tni mod12 4 [1,5,6] == [3,11,10]
-- > (z_sro_invert mod12 0 . z_sro_tn mod12 4) [1,5,6] == [7,3,2]
z_sro_tni :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_tni z n = z_sro_tn z n . z_sro_invert z 0

-- | Modulo multiplication.
--
-- > z_sro_mn mod12 11 [0,1,4,9] == z_tni mod12 0 [0,1,4,9]
z_sro_mn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_mn z n = fmap (z_mul z n)

-- | T-related sequences of /p/.
--
-- > length (z_sro_t_related mod12 [0,3,6,9]) == 12
-- > z_sro_t_related mod5 [0,2] == [[0,2],[1,3],[2,4],[3,0],[4,1]]
z_sro_t_related :: (Integral i, Functor f) => Z i -> f i -> [f i]
z_sro_t_related z p = fmap (\n -> z_sro_tn z n p) (z_univ z)

-- | T\/I-related sequences of /p/.
--
-- > length (z_sro_ti_related mod12 [0,1,3]) == 24
-- > length (z_sro_ti_related mod12 [0,3,6,9]) == 24
-- > z_sro_ti_related mod12 [0] == map return [0..11]
z_sro_ti_related :: (Eq (f i), Integral i, Functor f) => Z i -> f i -> [f i]
z_sro_ti_related z p = nub (z_sro_t_related z p ++ z_sro_t_related z (z_sro_invert z 0 p))

-- | R\/T\/I-related sequences of /p/.
--
-- > length (z_sro_rti_related mod12 [0,1,3]) == 48
-- > length (z_sro_rti_related mod12 [0,3,6,9]) == 24
z_sro_rti_related :: Integral i => Z i -> [i] -> [[i]]
z_sro_rti_related z p = let q = z_sro_ti_related z p in nub (q ++ map reverse q)

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > z_sro_tn_to mod12 5 [0,1,3] == [5,6,8]
-- > map (z_sro_tn_to mod12 0) [[0,1,3],[1,3,0],[3,0,1]]
z_sro_tn_to :: Integral i => Z i -> i -> [i] -> [i]
z_sro_tn_to z n p =
    case p of
      [] -> []
      x:xs -> n : z_sro_tn z (z_sub z n x) xs

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (z_sro_invert_ix mod12 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (z_sro_invert_ix mod12 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
z_sro_invert_ix :: Integral i => Z i -> Int -> [i] -> [i]
z_sro_invert_ix z n p = z_sro_invert z (p !! n) p

-- | The standard t-matrix of /p/.
--
-- > z_tmatrix mod12 [0,1,3] == [[0,1,3],[11,0,2],[9,10,0]]
z_tmatrix :: Integral i => Z i -> [i] -> [[i]]
z_tmatrix z p = map (\n -> z_sro_tn z n p) (z_sro_tn_to z 0 (z_sro_invert_ix z 0 p))
