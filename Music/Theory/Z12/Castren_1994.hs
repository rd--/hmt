-- | Marcus Castrén. /RECREL: A Similarity Measure for Set-Classes/. PhD
-- thesis, Sibelius Academy, Helsinki, 1994.
module Music.Theory.Z12.Castren_1994 where

import Data.List
import Data.Maybe
import qualified Music.Theory.List as L
import Music.Theory.Z12
import Music.Theory.Z12.TTO
import qualified Music.Theory.Z12.Forte_1973 as F

-- | Transpositional equivalence prime form, ie. 'F.t_cmp_prime' of
-- 'F.forte_cmp'.
--
-- > (F.forte_prime [0,2,3],t_prime [0,2,3]) == ([0,1,3],[0,2,3])
t_prime :: [Z12] -> [Z12]
t_prime = F.t_cmp_prime F.forte_cmp

-- | Is /p/ symmetrical under inversion.
--
-- > map inv_sym (F.scs_n 2) == [True,True,True,True,True,True]
-- > map (fromEnum.inv_sym) (F.scs_n 3) == [1,0,0,0,0,1,0,0,1,1,0,1]
inv_sym :: [Z12] -> Bool
inv_sym x = x `elem` map (\i -> sort (tn i (invert 0 x))) [0..11]

-- | If /p/ is not 'inv_sym' then @(p,invert 0 p)@ else 'Nothing'.
--
-- > sc_t_ti [0,2,4] == Nothing
-- > sc_t_ti [0,1,3] == Just ([0,1,3],[0,2,3])
sc_t_ti :: [Z12] -> Maybe ([Z12], [Z12])
sc_t_ti p =
    if inv_sym p
    then Nothing
    else Just (p,t_prime (invert 0 p))

-- | Transpositional equivalence variant of Forte's 'F.sc_table'.  The
-- inversionally related classes are distinguished by labels @A@ and
-- @B@; the class providing the /best normal order/ (Forte 1973) is
-- always the @A@ class. If neither @A@ nor @B@ appears in the name of
-- a set-class, it is inversionally symmetrical.
--
-- > (length F.sc_table,length t_sc_table) == (224,352)
-- > lookup "5-Z18B" t_sc_table == Just [0,2,3,6,7]
t_sc_table :: [(F.SC_Name,[Z12])]
t_sc_table =
    let f x = let nm = F.sc_name x
              in case sc_t_ti x of
                   Nothing -> [(nm,x)]
                   Just (p,q) -> [(nm++"A",p),(nm++"B",q)]
    in concatMap f F.scs

-- | Lookup a set-class name.  The input set is subject to
-- 't_prime' before lookup.
--
-- > t_sc_name [0,2,3,6,7] == "5-Z18B"
-- > t_sc_name [0,1,4,6,7,8] == "6-Z17B"
t_sc_name :: [Z12] -> F.SC_Name
t_sc_name p =
    let n = find (\(_,q) -> t_prime p == q) t_sc_table
    in fst (fromJust n)

-- | Lookup a set-class given a set-class name.
--
-- > t_sc "6-Z17A" == [0,1,2,4,7,8]
t_sc :: F.SC_Name -> [Z12]
t_sc n = snd (fromJust (find (\(m,_) -> n == m) t_sc_table))

-- | List of set classes.
t_scs :: [[Z12]]
t_scs = map snd t_sc_table

-- | Cardinality /n/ subset of 'scs'.
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
t_subsets x a = filter (`L.is_subset` x) (t_related a)

-- | T/I-related /q/ that are subsets of /p/.
--
-- > ti_subsets [0,1,2,3,4] [0,1]  == [[0,1],[1,2],[2,3],[3,4]]
-- > ti_subsets [0,1,2,3,4] [0,1,4] == [[0,1,4],[0,3,4]]
-- > ti_subsets [0,2,3,6,7] [0,1,4] == [[2,3,6],[3,6,7]]
ti_subsets :: [Z12] -> [Z12] -> [[Z12]]
ti_subsets x a = filter (`L.is_subset` x) (ti_related a)

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

-- | T/I-equivalence /n/-class vector (subset-class vector, nCV).
--
-- > ti_n_class_vector 2 [0..4] == [4,3,2,1,0,0]
-- > ti_n_class_vector 3 [0,1,2,3,4] == [3,4,2,0,0,1,0,0,0,0,0,0]
-- > rle (ti_n_class_vector 4 [0,1,2,3,4]) == [(2,2),(1,1),(26,0)]
ti_n_class_vector :: (Num b, Integral i) => i -> [Z12] -> [b]
ti_n_class_vector n x =
    let a = F.scs_n n
    in map (genericLength . ti_subsets x) a
