-- | Serial (ordered) pitch-class operations on 'Z'.
module Music.Theory.Z.SRO where

import Data.List {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as P {- parsec -}

import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Parse as Parse {- hmt -}

import Music.Theory.Z

-- | Serial operator,of the form rRTMI.
data SRO t = SRO {sro_r :: Int
                 ,sro_R :: Bool
                 ,sro_T :: t
                 ,sro_M :: t
                 ,sro_I :: Bool}
             deriving (Eq,Show)

-- | Printer in 'rnRTnMI' form.
sro_pp :: (Show t,Eq t,Num t) => SRO t -> String
sro_pp (SRO rN r tN m i) =
    concat [if rN /= 0 then 'r' : show rN else ""
           ,if r then "R" else ""
           ,'T' : show tN
           ,if m == 5 then "M" else if m == 1 then "" else error "sro_pp: M?"
           ,if i then "I" else ""]

-- | Parser for SRO.
p_sro :: Integral t => t -> P.GenParser Char () (SRO t)
p_sro m_mul = do
  let rot = P.option 0 (P.char 'r' >> Parse.parse_int)
  r <- rot
  r' <- Parse.is_char 'R'
  _ <- P.char 'T'
  t <- Parse.parse_int
  m <- Parse.is_char 'M'
  i <- Parse.is_char 'I'
  P.eof
  return (SRO r r' t (if m then m_mul else 1) i)

-- | Parse a Morris format serial operator descriptor.
--
-- > sro_parse 5 "r2RT3MI" == SRO 2 True 3 5 True
sro_parse :: Integral i => i -> String -> SRO i
sro_parse m =
    either (\e -> error ("sro_parse failed\n" ++ show e)) id .
    P.parse (p_sro m) ""

-- * Z

-- | The total set of serial operations.
--
-- > let u = z_sro_univ 3 5 z12
-- > zip (map sro_pp u) (map (\o -> z_sro_apply z12 o [0,1,3]) u)
z_sro_univ :: Integral i => Int -> i -> Z i -> [SRO i]
z_sro_univ n_rot m_mul z =
    [SRO r r' t m i |
     r <- [0 .. n_rot - 1],
     r' <- [False,True],
     t <- z_univ z,
     m <- [1,m_mul],
     i <- [False,True]]

-- | The set of transposition 'SRO's.
z_sro_Tn :: Integral i => Z i -> [SRO i]
z_sro_Tn z = [SRO 0 False n 1 False | n <- z_univ z]

-- | The set of transposition and inversion 'SRO's.
z_sro_TnI :: Integral i => Z i -> [SRO i]
z_sro_TnI z =
    [SRO 0 False n 1 i |
     n <- z_univ z,
     i <- [False,True]]

-- | The set of retrograde and transposition and inversion 'SRO's.
z_sro_RTnI :: Integral i => Z i -> [SRO i]
z_sro_RTnI z =
    [SRO 0 r n 1 i |
     r <- [True,False],
     n <- z_univ z,
     i <- [False,True]]

-- | The set of transposition, @M@ and inversion 'SRO's.
z_sro_TnMI :: Integral i => i -> Z i -> [SRO i]
z_sro_TnMI m_mul z =
    [SRO 0 False n m i |
     n <- z_univ z,
     m <- [1,m_mul],
     i <- [True,False]]

-- | The set of retrograde,transposition,@M5@ and inversion 'SRO's.
z_sro_RTnMI :: Integral i => i -> Z i -> [SRO i]
z_sro_RTnMI m_mul z =
    [SRO 0 r n m i |
     r <- [True,False],
     n <- z_univ z,
     m <- [1,m_mul],
     i <- [True,False]]

-- * Serial operations

-- | Apply SRO.
--
-- > z_sro_apply z12 (SRO 1 True 1 5 False) [0,1,2,3] == [11,6,1,4]
-- > z_sro_apply z12 (SRO 1 False 4 5 True) [0,1,2,3] == [11,6,1,4]
z_sro_apply :: Integral i => Z i -> SRO i -> [i] -> [i]
z_sro_apply z (SRO r r' t m i) x =
    let x1 = if i then z_sro_invert z 0 x else x
        x2 = if m == 1 then x1 else z_sro_mn z m x1
        x3 = z_sro_tn z t x2
        x4 = if r' then reverse x3 else x3
    in List.rotate_left r x4

-- * PLAIN

-- | Transpose /p/ by /n/.
--
-- > z_sro_tn z5 4 [0,1,4] == [4,0,3]
-- > z_sro_tn z12 4 [1,5,6] == [5,9,10]
z_sro_tn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_tn z n = fmap (z_add z n)

-- | Invert /p/ about /n/.
--
-- > z_sro_invert z5 0 [0,1,4] == [0,4,1]
-- > z_sro_invert z12 6 [4,5,6] == [8,7,6]
-- > map (z_sro_invert z12 0) [[0,1,3],[1,4,8]] == [[0,11,9],[11,8,4]]
--
-- > import Data.Word {- base -}
-- > z_sro_invert z12 (0::Word8) [1,4,8] == [3,0,8]
z_sro_invert :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_invert z n = fmap (\p -> z_sub z n (z_sub z p  n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > z_sro_tni z5 1 [0,1,3] == [1,0,3]
-- > z_sro_tni z12 4 [1,5,6] == [3,11,10]
-- > (z_sro_invert z12 0 . z_sro_tn z12 4) [1,5,6] == [7,3,2]
z_sro_tni :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_tni z n = z_sro_tn z n . z_sro_invert z 0

-- | Modulo multiplication.
--
-- > z_sro_mn z12 11 [0,1,4,9] == z_sro_tni z12 0 [0,1,4,9]
z_sro_mn :: (Integral i, Functor f) => Z i -> i -> f i -> f i
z_sro_mn z n = fmap (z_mul z n)

-- | M5, ie. 'mn' @5@.
--
-- > z_sro_m5 z12 [0,1,3] == [0,5,3]
z_sro_m5 :: (Integral i, Functor f) => Z i -> f i -> f i
z_sro_m5 z = z_sro_mn z 5

-- | T-related sequences of /p/.
--
-- > length (z_sro_t_related z12 [0,3,6,9]) == 12
-- > z_sro_t_related z5 [0,2] == [[0,2],[1,3],[2,4],[3,0],[4,1]]
z_sro_t_related :: (Integral i, Functor f) => Z i -> f i -> [f i]
z_sro_t_related z p = fmap (\n -> z_sro_tn z n p) (z_univ z)

-- | T\/I-related sequences of /p/.
--
-- > length (z_sro_ti_related z12 [0,1,3]) == 24
-- > length (z_sro_ti_related z12 [0,3,6,9]) == 24
-- > z_sro_ti_related z12 [0] == map return [0..11]
z_sro_ti_related :: (Eq (f i), Integral i, Functor f) => Z i -> f i -> [f i]
z_sro_ti_related z p = nub (z_sro_t_related z p ++ z_sro_t_related z (z_sro_invert z 0 p))

-- | R\/T\/I-related sequences of /p/.
--
-- > length (z_sro_rti_related z12 [0,1,3]) == 48
-- > length (z_sro_rti_related z12 [0,3,6,9]) == 24
z_sro_rti_related :: Integral i => Z i -> [i] -> [[i]]
z_sro_rti_related z p = let q = z_sro_ti_related z p in nub (q ++ map reverse q)

-- | T\/M\/I-related sequences of /p/, duplicates removed.
z_sro_tmi_related :: Integral i => Z i -> [i] -> [[i]]
z_sro_tmi_related z p = let q = z_sro_ti_related z p in nub (q ++ map (z_sro_m5 z) q)

-- | R\/T\/M\/I-related sequences of /p/, duplicates removed.
z_sro_rtmi_related :: Integral i => Z i -> [i] -> [[i]]
z_sro_rtmi_related z p = let q = z_sro_tmi_related z p in nub (q ++ map reverse q)

-- | r\/R\/T\/M\/I-related sequences of /p/, duplicates removed.
z_sro_rrtmi_related :: Integral i => Z i -> [i] -> [[i]]
z_sro_rrtmi_related z p = nub (concatMap (z_sro_rtmi_related z) (List.rotations p))

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > z_sro_tn_to z12 5 [0,1,3] == [5,6,8]
-- > map (z_sro_tn_to z12 0) [[0,1,3],[1,3,0],[3,0,1]] == [[0,1,3],[0,2,11],[0,9,10]]
z_sro_tn_to :: Integral i => Z i -> i -> [i] -> [i]
z_sro_tn_to z n p =
    case p of
      [] -> []
      x:xs -> n : z_sro_tn z (z_sub z n x) xs

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (z_sro_invert_ix z12 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (z_sro_invert_ix z12 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
z_sro_invert_ix :: Integral i => Z i -> Int -> [i] -> [i]
z_sro_invert_ix z n p = z_sro_invert z (p !! n) p

-- | The standard t-matrix of /p/.
--
-- > z_tmatrix z12 [0,1,3] == [[0,1,3],[11,0,2],[9,10,0]]
z_tmatrix :: Integral i => Z i -> [i] -> [[i]]
z_tmatrix z p = map (\n -> z_sro_tn z n p) (z_sro_tn_to z 0 (z_sro_invert_ix z 0 p))
