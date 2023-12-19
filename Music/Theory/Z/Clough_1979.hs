{- | John Clough. "Aspects of Diatonic Sets".
_Journal of Music Theory_, 23(1):45--61, 1979.
-}
module Music.Theory.Z.Clough_1979 where

import Data.List {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import Music.Theory.Z {- hmt -}

{- | Shift sequence so the initial value is zero.

> transpose_to_zero [1,2,5] == [0,1,4]
-}
transpose_to_zero :: Num n => [n] -> [n]
transpose_to_zero p =
  case p of
    [] -> []
    n : _ -> map (subtract n) p

{- | Diatonic pitch class (Z7) set to /chord/.

> map dpcset_to_chord [[0,1],[0,2,4],[2,3,4,5,6]] == [[1,6],[2,2,3],[1,1,1,1,3]]
-}
dpcset_to_chord :: Integral n => [n] -> [n]
dpcset_to_chord = List.d_dx . (++ [7]) . transpose_to_zero . nub . sort

{- | Inverse of 'dpcset_to_chord'.

> map chord_to_dpcset [[1,6],[2,2,3]] == [[0,1],[0,2,4]]
-}
chord_to_dpcset :: Integral n => [n] -> [n]
chord_to_dpcset = List.dropRight 1 . List.dx_d 0

{- | Complement, ie. in relation to 'z7_univ'.

> map dpcset_complement [[0,1],[0,2,4]] == [[2,3,4,5,6],[1,3,5,6]]
-}
dpcset_complement :: Integral n => [n] -> [n]
dpcset_complement p = filter (`notElem` p) z7_univ

{- | Interval class predicate (ie. 'is_z4').

> map is_ic [-1 .. 4] == [False,True,True,True,True,False]
-}
is_ic :: Integral n => n -> Bool
is_ic = is_z4

{- | Interval to interval class.

> map i_to_ic [0..7] == [0,1,2,3,3,2,1,0]
-}
i_to_ic :: Integral n => n -> n
i_to_ic n = if n > 3 then 7 - n else n

{- | Is /chord/, ie. is 'sum' @7@.

> is_chord [2,2,3]
-}
is_chord :: Integral n => [n] -> Bool
is_chord = (== 7) . sum

{- | Interval vector, given list of intervals.

> iv [2,2,3] == [0,2,1]
-}
iv :: Integral n => [n] -> [n]
iv p =
  let h = List.generic_histogram p
      f n = List.lookup_def n 0 h
  in map f [1, 2, 3]

-- | Comparison function for 'inv'.
inf_cmp :: Ord a => [a] -> [a] -> Ordering
inf_cmp p q =
  if null p && null q
    then EQ
    else case compare (last p) (last q) of
      EQ -> inf_cmp (List.dropRight 1 p) (List.dropRight 1 q)
      r -> r

{- | Interval normal form.

> map inf [[2,2,3],[1,2,4],[2,1,4]] == [[2,2,3],[1,2,4],[2,1,4]]
-}
inf :: Integral n => [n] -> [n]
inf = maximumBy inf_cmp . List.rotations

{- | Inverse of chord (retrograde).

> let p = [1,2,4] in (inf p,invert p,inf (invert p)) == ([1,2,4],[4,2,1],[2,1,4])
-}
invert :: [n] -> [n]
invert = reverse

{- | Complement of /chord/.

> let r = [[1,1,1,1,3],[1,1,1,2,2],[1,1,2,1,2],[1,1,1,4],[2,1,1,3],[1,2,1,3],[1,2,2,2]]
> in map complement [[1,6],[2,5],[3,4],[1,1,5],[1,2,4],[1,3,3],[2,2,3]] == r
-}
complement :: Integral n => [n] -> [n]
complement = inf . dpcset_to_chord . dpcset_complement . chord_to_dpcset

{- | Z7 pitch sequence to Z7 interval sequence, ie. 'mod7' of 'List.d_dx'.

> map iseq (permutations [0,1,2]) == [[1,1],[6,2],[6,6],[1,5],[5,1],[2,6]]
> map iseq (permutations [0,1,3]) == [[1,2],[6,3],[5,6],[2,4],[4,1],[3,5]]
> map iseq (permutations [0,2,3]) == [[2,1],[5,3],[6,5],[1,4],[4,2],[3,6]]
> map iseq (permutations [0,1,4]) == [[1,3],[6,4],[4,6],[3,3],[3,1],[4,4]]
> map iseq (permutations [0,2,4]) == [[2,2],[5,4],[5,5],[2,3],[3,2],[4,5]]
-}
iseq :: Integral n => [n] -> [n]
iseq = map mod7 . List.d_dx

-- * Z

-- | Z /m/ universe, ie [0 .. m-1].
z_n_univ :: Integral n => n -> [n]
z_n_univ m = [0 .. m - 1]

-- | 'is_z_n' of 4.
is_z4 :: Integral n => n -> Bool
is_z4 = is_z_n 4

{- | 'z_n_univ' of 7.

> z7_univ == [0 .. 6]
-}
z7_univ :: Integral n => [n]
z7_univ = z_n_univ 7

-- | 'is_z_n' of 7.
is_z7 :: Integral n => n -> Bool
is_z7 = is_z_n 7

-- | 'mod' 7.
mod7 :: Integral n => n -> n
mod7 n = n `mod` 7
