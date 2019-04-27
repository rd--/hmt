-- | Marcus Castrén. /RECREL: A Similarity Measure for Set-Classes/. PhD
-- thesis, Sibelius Academy, Helsinki, 1994.
module Music.Theory.Z.Castren_1994 where

import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as List
import Music.Theory.Z (mod12)
import qualified Music.Theory.Z.Forte_1973 as Forte
import qualified Music.Theory.Z.SRO as SRO

type Z12 = Int8

-- | Is /p/ symmetrical under inversion.
--
-- > map inv_sym (Forte.scs_n 2) == [True,True,True,True,True,True]
-- > map (fromEnum.inv_sym) (Forte.scs_n 3) == [1,0,0,0,0,1,0,0,1,1,0,1]
inv_sym :: [Z12] -> Bool
inv_sym x = x `elem` map (\i -> sort (SRO.z_sro_tn mod12 i (SRO.z_sro_invert mod12 0 x))) [0..11]

-- | If /p/ is not 'inv_sym' then @(p,invert 0 p)@ else 'Nothing'.
--
-- > sc_t_ti [0,2,4] == Nothing
-- > sc_t_ti [0,1,3] == Just ([0,1,3],[0,2,3])
sc_t_ti :: [Z12] -> Maybe ([Z12], [Z12])
sc_t_ti p =
    if inv_sym p
    then Nothing
    else Just (p,Forte.t_prime mod12 (SRO.z_sro_invert mod12 0 p))

-- | Transpositional equivalence variant of Forte's 'sc_table'.  The
-- inversionally related classes are distinguished by labels @A@ and
-- @B@; the class providing the /best normal order/ (Forte 1973) is
-- always the @A@ class. If neither @A@ nor @B@ appears in the name of
-- a set-class, it is inversionally symmetrical.
--
-- > (length Forte.sc_table,length t_sc_table) == (224,352)
-- > lookup "5-Z18B" t_sc_table == Just [0,2,3,6,7]
t_sc_table :: [(Forte.SC_Name,[Z12])]
t_sc_table =
    let f x = let nm = Forte.sc_name mod12 x
              in case sc_t_ti x of
                   Nothing -> [(nm,x)]
                   Just (p,q) -> [(nm++"A",p),(nm++"B",q)]
    in concatMap f Forte.scs

-- | Lookup a set-class name.  The input set is subject to
-- 't_prime' before lookup.
--
-- > t_sc_name [0,2,3,6,7] == "5-Z18B"
-- > t_sc_name [0,1,4,6,7,8] == "6-Z17B"
t_sc_name :: [Z12] -> Forte.SC_Name
t_sc_name p =
    let n = find (\(_,q) -> Forte.t_prime mod12 p == q) t_sc_table
    in fst (fromJust n)

-- | Lookup a set-class given a set-class name.
--
-- > t_sc "6-Z17A" == [0,1,2,4,7,8]
t_sc :: Forte.SC_Name -> [Z12]
t_sc n = snd (fromJust (find (\(m,_) -> n == m) t_sc_table))

-- | List of set classes.
t_scs :: [[Z12]]
t_scs = map snd t_sc_table

-- | Cardinality /n/ subset of 't_scs'.
--
-- > map (length . t_scs_n) [2..10] == [6,19,43,66,80,66,43,19,6]
t_scs_n :: Integral i => i -> [[Z12]]
t_scs_n n = filter ((== n) . genericLength) t_scs

-- | T-related /q/ that are subsets of /p/.
--
-- > t_subsets [0,1,2,3,4] [0,1]  == [[0,1],[1,2],[2,3],[3,4]]
-- > t_subsets [0,1,2,3,4] [0,1,4] == [[0,1,4]]
-- > t_subsets [0,2,3,6,7] [0,1,4] == [[2,3,6]]
t_subsets :: [Z12] -> [Z12] -> [[Z12]]
t_subsets x a = filter (`List.is_subset` x) (map sort (SRO.z_sro_t_related mod12 a))

-- | T\/I-related /q/ that are subsets of /p/.
--
-- > ti_subsets [0,1,2,3,4] [0,1]  == [[0,1],[1,2],[2,3],[3,4]]
-- > ti_subsets [0,1,2,3,4] [0,1,4] == [[0,1,4],[0,3,4]]
-- > ti_subsets [0,2,3,6,7] [0,1,4] == [[2,3,6],[3,6,7]]
ti_subsets :: [Z12] -> [Z12] -> [[Z12]]
ti_subsets x a = filter (`List.is_subset` x) (nub (map sort (SRO.z_sro_ti_related mod12 a)))

-- | Trivial run length encoder.
--
-- > rle "abbcccdde" == [(1,'a'),(2,'b'),(3,'c'),(2,'d'),(1,'e')]
rle :: (Eq a,Integral i) => [a] -> [(i,a)]
rle =
    let f x = (genericLength x,head x)
    in map f . group

-- | Inverse of 'rle'.
--
-- > rle_decode [(5,'a'),(4,'b')] == "aaaaabbbb"
rle_decode :: (Integral i) => [(i,a)] -> [a]
rle_decode =
    let f (i,j) = genericReplicate i j
    in concatMap f

-- | Length of /rle/ encoded sequence.
--
-- > rle_length [(5,'a'),(4,'b')] == 9
rle_length :: (Integral i) => [(i,a)] -> i
rle_length = sum . map fst

-- | T-equivalence /n/-class vector (subset-class vector, nCV).
--
-- > t_n_class_vector 2 [0..4] == [4,3,2,1,0,0]
-- > rle (t_n_class_vector 3 [0..4]) == [(1,3),(2,2),(2,1),(4,0),(1,1),(9,0)]
-- > rle (t_n_class_vector 4 [0..4]) == [(1,2),(3,1),(39,0)]
t_n_class_vector :: (Num a, Integral i) => i -> [Z12] -> [a]
t_n_class_vector n x =
    let a = t_scs_n n
    in map (genericLength . t_subsets x) a

-- | T\/I-equivalence /n/-class vector (subset-class vector, nCV).
--
-- > ti_n_class_vector 2 [0..4] == [4,3,2,1,0,0]
-- > ti_n_class_vector 3 [0,1,2,3,4] == [3,4,2,0,0,1,0,0,0,0,0,0]
-- > rle (ti_n_class_vector 4 [0,1,2,3,4]) == [(2,2),(1,1),(26,0)]
ti_n_class_vector :: (Num b, Integral i) => i -> [Z12] -> [b]
ti_n_class_vector n x =
    let a = Forte.scs_n n
    in map (genericLength . ti_subsets x) a

-- | 'icv' scaled by sum of /icv/.
--
-- > dyad_class_percentage_vector [0,1,2,3,4] == [40,30,20,10,0,0]
-- > dyad_class_percentage_vector [0,1,4,5,7] == [20,10,20,20,20,10]
dyad_class_percentage_vector :: Integral i => [Z12] -> [i]
dyad_class_percentage_vector p =
    let p' = Forte.icv 12 p
    in map (sum p' *) p'

-- | /rel/ metric.
--
-- > rel [0,1,2,3,4] [0,1,4,5,7] == 40
-- > rel [0,1,2,3,4] [0,2,4,6,8] == 60
-- > rel [0,1,4,5,7] [0,2,4,6,8] == 60
rel :: Integral i => [Z12] -> [Z12] -> Ratio i
rel x y =
    let x' = dyad_class_percentage_vector x
        y' = dyad_class_percentage_vector y
    in sum (map abs (zipWith (-) x' y')) % 2
