-- | Allen Forte. /The Structure of Atonal Music/.
--   Yale University Press, New Haven, 1973.
module Music.Theory.Z.Forte_1973 where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Set.List as S {- hmt -}

import Music.Theory.Unicode {- hmt -}
import Music.Theory.Z {- hmt -}
import Music.Theory.Z.SRO {- hmt -}

-- * Prime form

-- | T-related rotations of /p/.
--
-- > t_rotations mod12 [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]]
t_rotations :: Integral i => Z i -> [i] -> [[i]]
t_rotations z p =
    let r = T.rotations (sort p)
    in map (z_sro_tn_to z 0) r

-- | T\/I-related rotations of /p/.
--
-- > ti_rotations mod12 [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]
-- >                               ,[0,9,11],[0,2,3],[0,1,10]]
ti_rotations :: Integral i => Z i -> [i] -> [[i]]
ti_rotations z p =
    let q = z_sro_invert z 0 p
        r = T.rotations (sort p) ++ T.rotations (sort q)
    in map (z_sro_tn_to z 0) r

-- | Prime form rule requiring comparator, considering 't_rotations'.
t_cmp_prime :: Integral i => Z i -> ([i] -> [i] -> Ordering) -> [i] -> [i]
t_cmp_prime z f = T.minimumBy_or [] f . t_rotations z

-- | Prime form rule requiring comparator, considering 'ti_rotations'.
ti_cmp_prime :: Integral i => Z i -> ([i] -> [i] -> Ordering) -> [i] -> [i]
ti_cmp_prime z f = T.minimumBy_or [] f . ti_rotations z

-- | Forte comparison function (rightmost first then leftmost outwards).
--
-- > forte_cmp [0,1,3,6,8,9] [0,2,3,6,7,9] == LT
forte_cmp :: (Ord t) => [t] -> [t] -> Ordering
forte_cmp p  q  =
    case (p,q) of
      ([],[]) -> EQ
      ([],_) -> LT
      (_,[]) -> GT
      _ -> let r = compare (last p) (last q)
           in if r == EQ then compare p q else r

-- | Forte prime form, ie. 'ti_cmp_prime' of 'forte_cmp'.
--
-- > forte_prime mod12 [0,1,3,6,8,9] == [0,1,3,6,8,9]
-- > forte_prime mod5 [0,1,4] == [0,1,2]
--
-- > S.set (map (forte_prime mod5) (S.powerset [0..4]))
-- > S.set (map (forte_prime mod7) (S.powerset [0..6]))
forte_prime :: Integral i => Z i -> [i] -> [i]
forte_prime z = ti_cmp_prime z forte_cmp

-- | Transpositional equivalence prime form, ie. 't_cmp_prime' of
-- 'forte_cmp'.
--
-- > (forte_prime mod12 [0,2,3],t_prime mod12 [0,2,3]) == ([0,1,3],[0,2,3])
t_prime :: Integral i => Z i -> [i] -> [i]
t_prime z = t_cmp_prime z forte_cmp

-- * ICV Metric

-- | Interval class of interval /i/.
--
-- > map (ic 12) [0..11] == [0,1,2,3,4,5,6,5,4,3,2,1]
-- > map (ic 7) [0..6] == [0,1,2,3,3,2,1]
-- > map (ic 5) [1,2,3,4] == [1,2,2,1]
-- > map (ic 12) [5,6,7] == [5,6,5]
-- > map (ic 12 . to_Z mod12) [-13,-1,0,1,13] == [1,1,0,1,1]
ic :: Integral i => i -> i -> i
ic z i = if i <= (z `div` 2) then i else z - i

-- | Forte notation for interval class vector.
--
-- > icv 12 [0,1,2,4,7,8] == [3,2,2,3,3,2]
icv :: (Integral i, Num n) => i -> [i] -> [n]
icv z s =
    let i = map (ic z . flip mod z . uncurry (-)) (S.pairs s)
        f l = (head l,genericLength l)
        j = map f (group (sort i))
        k = map (`lookup` j) [1 .. z `div` 2]
    in map (fromMaybe 0) k

-- * BIP Metric

-- | Basic interval pattern, see Allen Forte \"The Basic Interval Patterns\"
-- /JMT/ 17/2 (1973):234-272
--
-- >>> bip 0t95728e3416
-- 11223344556
--
-- > bip 12 [0,10,9,5,7,2,8,11,3,4,1,6] == [1,1,2,2,3,3,4,4,5,5,6]
bip :: Integral a => a -> [a] -> [a]
bip z = sort . map (ic z . flip mod z) . T.d_dx

-- * Name

{- | Generate SC universe, though not in order of the Forte table.

> let r = [[]
>         ,[0]
>         ,[0,1],[0,2],[0,3]
>         ,[0,1,2],[0,1,3],[0,1,4],[0,2,4]
>         ,[0,1,2,3],[0,1,2,4],[0,1,3,4],[0,1,3,5]
>         ,[0,1,2,3,4],[0,1,2,3,5],[0,1,2,4,5]
>         ,[0,1,2,3,4,5]
>         ,[0,1,2,3,4,5,6]]
> in sc_univ mod7 == r

> sort (sc_univ mod12) == sort (map snd sc_table)

> zipWith (\p q -> (p == q,p,q)) (sc_univ mod12) (map snd sc_table)

-}
sc_univ :: Integral i => Z i -> [[i]]
sc_univ z =
    T.sort_by_two_stage length id $
    nub $
    map (forte_prime z) $
    S.powerset (z_univ z)

-- | Synonym for 'String'.
type SC_Name = String

-- | Table of (SC-NAME,PCSET).
type SC_Table n = [(SC_Name,[n])]

-- | The set-class table (Forte prime forms).
--
-- > length sc_table == 224
sc_table :: Num n => SC_Table n
sc_table =
    [("0-1",[])
    ,("1-1",[0])
    ,("2-1",[0,1])
    ,("2-2",[0,2])
    ,("2-3",[0,3])
    ,("2-4",[0,4])
    ,("2-5",[0,5])
    ,("2-6",[0,6])
    ,("3-1",[0,1,2])
    ,("3-2",[0,1,3])
    ,("3-3",[0,1,4])
    ,("3-4",[0,1,5])
    ,("3-5",[0,1,6])
    ,("3-6",[0,2,4])
    ,("3-7",[0,2,5])
    ,("3-8",[0,2,6])
    ,("3-9",[0,2,7])
    ,("3-10",[0,3,6])
    ,("3-11",[0,3,7])
    ,("3-12",[0,4,8])
    ,("4-1",[0,1,2,3])
    ,("4-2",[0,1,2,4])
    ,("4-3",[0,1,3,4])
    ,("4-4",[0,1,2,5])
    ,("4-5",[0,1,2,6])
    ,("4-6",[0,1,2,7])
    ,("4-7",[0,1,4,5])
    ,("4-8",[0,1,5,6])
    ,("4-9",[0,1,6,7])
    ,("4-10",[0,2,3,5])
    ,("4-11",[0,1,3,5])
    ,("4-12",[0,2,3,6])
    ,("4-13",[0,1,3,6])
    ,("4-14",[0,2,3,7])
    ,("4-Z15",[0,1,4,6])
    ,("4-16",[0,1,5,7])
    ,("4-17",[0,3,4,7])
    ,("4-18",[0,1,4,7])
    ,("4-19",[0,1,4,8])
    ,("4-20",[0,1,5,8])
    ,("4-21",[0,2,4,6])
    ,("4-22",[0,2,4,7])
    ,("4-23",[0,2,5,7])
    ,("4-24",[0,2,4,8])
    ,("4-25",[0,2,6,8])
    ,("4-26",[0,3,5,8])
    ,("4-27",[0,2,5,8])
    ,("4-28",[0,3,6,9])
    ,("4-Z29",[0,1,3,7])
    ,("5-1",[0,1,2,3,4])
    ,("5-2",[0,1,2,3,5])
    ,("5-3",[0,1,2,4,5])
    ,("5-4",[0,1,2,3,6])
    ,("5-5",[0,1,2,3,7])
    ,("5-6",[0,1,2,5,6])
    ,("5-7",[0,1,2,6,7])
    ,("5-8",[0,2,3,4,6])
    ,("5-9",[0,1,2,4,6])
    ,("5-10",[0,1,3,4,6])
    ,("5-11",[0,2,3,4,7])
    ,("5-Z12",[0,1,3,5,6])
    ,("5-13",[0,1,2,4,8])
    ,("5-14",[0,1,2,5,7])
    ,("5-15",[0,1,2,6,8])
    ,("5-16",[0,1,3,4,7])
    ,("5-Z17",[0,1,3,4,8])
    ,("5-Z18",[0,1,4,5,7])
    ,("5-19",[0,1,3,6,7])
    ,("5-20",[0,1,3,7,8])
    ,("5-21",[0,1,4,5,8])
    ,("5-22",[0,1,4,7,8])
    ,("5-23",[0,2,3,5,7])
    ,("5-24",[0,1,3,5,7])
    ,("5-25",[0,2,3,5,8])
    ,("5-26",[0,2,4,5,8])
    ,("5-27",[0,1,3,5,8])
    ,("5-28",[0,2,3,6,8])
    ,("5-29",[0,1,3,6,8])
    ,("5-30",[0,1,4,6,8])
    ,("5-31",[0,1,3,6,9])
    ,("5-32",[0,1,4,6,9])
    ,("5-33",[0,2,4,6,8])
    ,("5-34",[0,2,4,6,9])
    ,("5-35",[0,2,4,7,9])
    ,("5-Z36",[0,1,2,4,7])
    ,("5-Z37",[0,3,4,5,8])
    ,("5-Z38",[0,1,2,5,8])
    ,("6-1",[0,1,2,3,4,5])
    ,("6-2",[0,1,2,3,4,6])
    ,("6-Z3",[0,1,2,3,5,6])
    ,("6-Z4",[0,1,2,4,5,6])
    ,("6-5",[0,1,2,3,6,7])
    ,("6-Z6",[0,1,2,5,6,7])
    ,("6-7",[0,1,2,6,7,8])
    ,("6-8",[0,2,3,4,5,7])
    ,("6-9",[0,1,2,3,5,7])
    ,("6-Z10",[0,1,3,4,5,7])
    ,("6-Z11",[0,1,2,4,5,7])
    ,("6-Z12",[0,1,2,4,6,7])
    ,("6-Z13",[0,1,3,4,6,7])
    ,("6-14",[0,1,3,4,5,8])
    ,("6-15",[0,1,2,4,5,8])
    ,("6-16",[0,1,4,5,6,8])
    ,("6-Z17",[0,1,2,4,7,8])
    ,("6-18",[0,1,2,5,7,8])
    ,("6-Z19",[0,1,3,4,7,8])
    ,("6-20",[0,1,4,5,8,9])
    ,("6-21",[0,2,3,4,6,8])
    ,("6-22",[0,1,2,4,6,8])
    ,("6-Z23",[0,2,3,5,6,8])
    ,("6-Z24",[0,1,3,4,6,8])
    ,("6-Z25",[0,1,3,5,6,8])
    ,("6-Z26",[0,1,3,5,7,8])
    ,("6-27",[0,1,3,4,6,9])
    ,("6-Z28",[0,1,3,5,6,9])
    ,("6-Z29",[0,1,3,6,8,9])
    ,("6-30",[0,1,3,6,7,9])
    ,("6-31",[0,1,3,5,8,9])
    ,("6-32",[0,2,4,5,7,9])
    ,("6-33",[0,2,3,5,7,9])
    ,("6-34",[0,1,3,5,7,9])
    ,("6-35",[0,2,4,6,8,10])
    ,("6-Z36",[0,1,2,3,4,7])
    ,("6-Z37",[0,1,2,3,4,8])
    ,("6-Z38",[0,1,2,3,7,8])
    ,("6-Z39",[0,2,3,4,5,8])
    ,("6-Z40",[0,1,2,3,5,8])
    ,("6-Z41",[0,1,2,3,6,8])
    ,("6-Z42",[0,1,2,3,6,9])
    ,("6-Z43",[0,1,2,5,6,8])
    ,("6-Z44",[0,1,2,5,6,9])
    ,("6-Z45",[0,2,3,4,6,9])
    ,("6-Z46",[0,1,2,4,6,9])
    ,("6-Z47",[0,1,2,4,7,9])
    ,("6-Z48",[0,1,2,5,7,9])
    ,("6-Z49",[0,1,3,4,7,9])
    ,("6-Z50",[0,1,4,6,7,9])
    ,("7-1",[0,1,2,3,4,5,6])
    ,("7-2",[0,1,2,3,4,5,7])
    ,("7-3",[0,1,2,3,4,5,8])
    ,("7-4",[0,1,2,3,4,6,7])
    ,("7-5",[0,1,2,3,5,6,7])
    ,("7-6",[0,1,2,3,4,7,8])
    ,("7-7",[0,1,2,3,6,7,8])
    ,("7-8",[0,2,3,4,5,6,8])
    ,("7-9",[0,1,2,3,4,6,8])
    ,("7-10",[0,1,2,3,4,6,9])
    ,("7-11",[0,1,3,4,5,6,8])
    ,("7-Z12",[0,1,2,3,4,7,9])
    ,("7-13",[0,1,2,4,5,6,8])
    ,("7-14",[0,1,2,3,5,7,8])
    ,("7-15",[0,1,2,4,6,7,8])
    ,("7-16",[0,1,2,3,5,6,9])
    ,("7-Z17",[0,1,2,4,5,6,9])
    ,("7-Z18",[0,1,2,3,5,8,9])
    ,("7-19",[0,1,2,3,6,7,9])
    ,("7-20",[0,1,2,4,7,8,9])
    ,("7-21",[0,1,2,4,5,8,9])
    ,("7-22",[0,1,2,5,6,8,9])
    ,("7-23",[0,2,3,4,5,7,9])
    ,("7-24",[0,1,2,3,5,7,9])
    ,("7-25",[0,2,3,4,6,7,9])
    ,("7-26",[0,1,3,4,5,7,9])
    ,("7-27",[0,1,2,4,5,7,9])
    ,("7-28",[0,1,3,5,6,7,9])
    ,("7-29",[0,1,2,4,6,7,9])
    ,("7-30",[0,1,2,4,6,8,9])
    ,("7-31",[0,1,3,4,6,7,9])
    ,("7-32",[0,1,3,4,6,8,9])
    ,("7-33",[0,1,2,4,6,8,10])
    ,("7-34",[0,1,3,4,6,8,10])
    ,("7-35",[0,1,3,5,6,8,10])
    ,("7-Z36",[0,1,2,3,5,6,8])
    ,("7-Z37",[0,1,3,4,5,7,8])
    ,("7-Z38",[0,1,2,4,5,7,8])
    ,("8-1",[0,1,2,3,4,5,6,7])
    ,("8-2",[0,1,2,3,4,5,6,8])
    ,("8-3",[0,1,2,3,4,5,6,9])
    ,("8-4",[0,1,2,3,4,5,7,8])
    ,("8-5",[0,1,2,3,4,6,7,8])
    ,("8-6",[0,1,2,3,5,6,7,8])
    ,("8-7",[0,1,2,3,4,5,8,9])
    ,("8-8",[0,1,2,3,4,7,8,9])
    ,("8-9",[0,1,2,3,6,7,8,9])
    ,("8-10",[0,2,3,4,5,6,7,9])
    ,("8-11",[0,1,2,3,4,5,7,9])
    ,("8-12",[0,1,3,4,5,6,7,9])
    ,("8-13",[0,1,2,3,4,6,7,9])
    ,("8-14",[0,1,2,4,5,6,7,9])
    ,("8-Z15",[0,1,2,3,4,6,8,9])
    ,("8-16",[0,1,2,3,5,7,8,9])
    ,("8-17",[0,1,3,4,5,6,8,9])
    ,("8-18",[0,1,2,3,5,6,8,9])
    ,("8-19",[0,1,2,4,5,6,8,9])
    ,("8-20",[0,1,2,4,5,7,8,9])
    ,("8-21",[0,1,2,3,4,6,8,10])
    ,("8-22",[0,1,2,3,5,6,8,10])
    ,("8-23",[0,1,2,3,5,7,8,10])
    ,("8-24",[0,1,2,4,5,6,8,10])
    ,("8-25",[0,1,2,4,6,7,8,10])
    ,("8-26",[0,1,2,4,5,7,9,10])
    ,("8-27",[0,1,2,4,5,7,8,10])
    ,("8-28",[0,1,3,4,6,7,9,10])
    ,("8-Z29",[0,1,2,3,5,6,7,9])
    ,("9-1",[0,1,2,3,4,5,6,7,8])
    ,("9-2",[0,1,2,3,4,5,6,7,9])
    ,("9-3",[0,1,2,3,4,5,6,8,9])
    ,("9-4",[0,1,2,3,4,5,7,8,9])
    ,("9-5",[0,1,2,3,4,6,7,8,9])
    ,("9-6",[0,1,2,3,4,5,6,8,10])
    ,("9-7",[0,1,2,3,4,5,7,8,10])
    ,("9-8",[0,1,2,3,4,6,7,8,10])
    ,("9-9",[0,1,2,3,5,6,7,8,10])
    ,("9-10",[0,1,2,3,4,6,7,9,10])
    ,("9-11",[0,1,2,3,5,6,7,9,10])
    ,("9-12",[0,1,2,4,5,6,8,9,10])
    ,("10-1",[0,1,2,3,4,5,6,7,8,9])
    ,("10-2",[0,1,2,3,4,5,6,7,8,10])
    ,("10-3",[0,1,2,3,4,5,6,7,9,10])
    ,("10-4",[0,1,2,3,4,5,6,8,9,10])
    ,("10-5",[0,1,2,3,4,5,7,8,9,10])
    ,("10-6",[0,1,2,3,4,6,7,8,9,10])
    ,("11-1",[0,1,2,3,4,5,6,7,8,9,10])
    ,("12-1",[0,1,2,3,4,5,6,7,8,9,10,11])]

-- | Unicode (non-breaking hyphen) variant.
sc_table_unicode :: Num n => SC_Table n
sc_table_unicode =
    let f = map (\c -> if c == '-' then non_breaking_hypen else c)
    in map (\(nm,pc) -> (f nm,pc)) sc_table

-- | Lookup name of prime form of set class.  It is an error for the
-- input not to be a forte prime form.
--
-- > forte_prime_name [0,1,4,6] == ("4-Z15",[0,1,4,6])
forte_prime_name :: (Num n,Eq n) => [n] -> (SC_Name,[n])
forte_prime_name p = fromMaybe (error "forte_prime_name") (find (\(_,q) -> p == q) sc_table)

-- | Lookup entry for set in table.
sc_tbl_lookup :: Integral i => Z i -> SC_Table i -> [i] -> Maybe (SC_Name,[i])
sc_tbl_lookup z tbl p = find (\(_,q) -> forte_prime z p == q) tbl

-- | Erroring variant
sc_tbl_lookup_err :: Integral i => Z i -> SC_Table i -> [i] -> (SC_Name,[i])
sc_tbl_lookup_err z tbl = fromMaybe (error "sc_tbl_lookup") . sc_tbl_lookup z tbl

-- | 'fst' of 'sc_tbl_lookup_err'
sc_name_tbl :: Integral i => Z i -> SC_Table i -> [i] -> SC_Name
sc_name_tbl z tbl = fst . sc_tbl_lookup_err z tbl

-- | Lookup a set-class name.  The input set is subject to
-- 'forte_prime' before lookup.
--
-- > sc_name mod12 [0,2,3,6,7] == "5-Z18"
-- > sc_name mod12 [0,1,4,6,7,8] == "6-Z17"
sc_name :: Integral i => Z i -> [i] -> SC_Name
sc_name z = sc_name_tbl z sc_table

-- | Long name (ie. with enumeration of prime form).
--
-- > sc_name_long mod12 [0,1,4,6,7,8] == "6-Z17[012478]"
sc_name_long :: Integral i => Z i -> [i] -> SC_Name
sc_name_long z p =
    let (nm,p') = sc_tbl_lookup_err z sc_table p
    in nm ++ z16_vec_pp p'

-- | Unicode (non-breaking hyphen) variant.
sc_name_unicode :: Integral i => Z i -> [i] -> SC_Name
sc_name_unicode z = sc_name_tbl z sc_table_unicode

-- | Lookup a set-class given a set-class name.
--
-- > sc "6-Z17" == [0,1,2,4,7,8]
sc :: Num n => SC_Name -> [n]
sc n = snd (fromMaybe (error "sc") (find (\(m,_) -> n == m) sc_table))

scs :: Num n => [[n]]
scs = map snd sc_table

-- | Cardinality /n/ subset of 'scs'.
--
-- > map (length . scs_n) [1..11] == [1,6,12,29,38,50,38,29,12,6,1]
scs_n :: (Integral i, Num n) => i -> [[n]]
scs_n n = filter ((== n) . genericLength) scs

-- | Vector indicating degree of intersection with inversion at each transposition.
--
-- > tics mod12 [0,2,4,5,7,9] == [3,2,5,0,5,2,3,4,1,6,1,4]
-- > map (tics mod12) scs
tics :: Integral i => Z i -> [i] -> [Int]
tics z p =
    let q = z_sro_t_related z (z_sro_invert z 0 p)
    in map (length . intersect p) q

-- * Z-relation

-- | Locate /Z/ relation of set class.
--
-- > fmap (sc_name mod12) (z_relation_of 12 (sc "7-Z12")) == Just "7-Z36"
z_relation_of :: Integral i => i -> [i] -> Maybe [i]
z_relation_of z x =
    let n = length x
        eq_i :: [Integer] -> [Integer] -> Bool
        eq_i = (==)
        f y = (x /= y) && (icv z x `eq_i` icv z y)
    in case filter f (scs_n n) of
         [] -> Nothing
         [r] -> Just r
         _ -> error "z_relation_of"
