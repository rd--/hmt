{- | \"Sieves\" by Iannis Xenakis and John Rahn
/Perspectives of New Music/
Vol. 28, No. 1 (Winter, 1990), pp. 58-78
-}
module Music.Theory.Xenakis.Sieve where

import qualified Data.List {- base -}

import qualified Music.Theory.List as List

-- | A Sieve.
data Sieve
  = -- | 'Empty' 'Sieve'
    Empty
  | -- | Primitive 'Sieve' of /modulo/ and /index/
    L (Integer, Integer)
  | -- | 'Union' of two 'Sieve's
    Union Sieve Sieve
  | -- | 'Intersection' of two 'Sieve's
    Intersection Sieve Sieve
  | -- | 'Complement' of a 'Sieve'
    Complement Sieve
  deriving (Eq, Show)

-- | The 'Union' of a list of 'Sieve's, ie. 'foldl1' 'Union'.
union :: [Sieve] -> Sieve
union = foldl1 Union

-- | The 'Intersection' of a list of 'Sieve's, ie. 'foldl1' 'Intersection'.
intersection :: [Sieve] -> Sieve
intersection = foldl1 Intersection

-- | Unicode synonym for 'Union'.
(∪) :: Sieve -> Sieve -> Sieve
(∪) = Union

-- | Unicode synonym for 'Intersection'.
(∩) :: Sieve -> Sieve -> Sieve
(∩) = Intersection

-- | Synonym for 'Complement'.
c :: Sieve -> Sieve
c = Complement

-- | Pretty-print sieve.  Fully parenthesised.
sieve_pp :: Sieve -> String
sieve_pp s =
  case s of
    Empty -> "∅"
    L (p, q) -> concat [show p, ".", show q]
    Union p q -> concat ["(", sieve_pp p, " ∪ ", sieve_pp q, ")"]
    Intersection p q -> concat ["(", sieve_pp p, " ∩ ", sieve_pp q, ")"]
    Complement p -> concat ["(∁ ", sieve_pp p, ")"]

{- | Variant of 'L', ie. 'curry' 'L'.

> l 15 19 == L (15,19)
-}
l :: Integer -> Integer -> Sieve
l = curry L

-- | unicode synonym for 'l'.
(⋄) :: Integer -> Integer -> Sieve
(⋄) = l

infixl 3 ∪
infixl 4 ∩
infixl 5 ⋄

{- | In a /normal/ 'Sieve' /m/ is '>' /i/.

>>> normalise (L (15,19))
L (15,4)

>>> normalise (L (11,13))
L (11,2)
-}
normalise :: Sieve -> Sieve
normalise s =
  case s of
    Empty -> Empty
    L (m, i) -> L (m, i `mod` m)
    Union s0 s1 -> Union (normalise s0) (normalise s1)
    Intersection s0 s1 -> Intersection (normalise s0) (normalise s1)
    Complement s' -> Complement (normalise s')

{- | Predicate to test if a 'Sieve' is /normal/.

>>> is_normal (L (15,4))
True

>>> is_normal (L (11,13))
False
-}
is_normal :: Sieve -> Bool
is_normal s = s == normalise s

{- | Predicate to determine if an 'I' is an element of the 'Sieve'.

>>> map (element (L (3,1))) [1..4]
[True,False,False,True]

>>> map (element (L (15,4))) [4,19 .. 49]
[True,True,True,True]
-}
element :: Sieve -> Integer -> Bool
element s n =
  case s of
    Empty -> False
    L (m, i) -> n `mod` m == i `mod` m && n >= i
    Union s0 s1 -> element s0 n || element s1 n
    Intersection s0 s1 -> element s0 n && element s1 n
    Complement s' -> not (element s' n)

{- | 'I' not in set.

>>> take 9 (i_complement [1,3..]) == [0,2..16]
True
-}
i_complement :: [Integer] -> [Integer]
i_complement =
  let f x s = case s of
        [] -> [x ..]
        e : s' -> case compare x e of
          LT -> x : f (x + 1) s
          EQ -> f (x + 1) s'
          GT -> error "i_complement"
  in f 0

{- | Construct the sequence defined by a 'Sieve'.  Note that building
     a sieve that contains an intersection clause that has no elements
     gives @_|_@.

>>> let d = [0,2,4,5,7,9,11]
>>> let r = d ++ map (+ 12) d
>>> take 14 (build (union (map (l 12) d))) == r
True
-}
build :: Sieve -> [Integer]
build s =
  let u_f = map List.head_err . Data.List.group
      i_f =
        let g [x, _] = [x]
            g _ = []
        in concatMap g . Data.List.group
  in case s of
      Empty -> []
      L (m, i) -> [i, i + m ..]
      Union s0 s1 -> u_f (List.merge (build s0) (build s1))
      Intersection s0 s1 -> i_f (List.merge (build s0) (build s1))
      Complement s' -> i_complement (build s')

{- | Variant of 'build' that gives the first /n/ places of the 'reduce' of 'Sieve'.

>>> buildn 6 (union (map (l 8) [0,3,6]))
[0,3,6,8,11,14]

>>> buildn 12 (L (3,2))
[2,5,8,11,14,17,20,23,26,29,32,35]

>>> buildn 9 (L (8,0))
[0,8,16,24,32,40,48,56,64]

>>> buildn 3 (L (3,2) ∩ L (8,0))
[8,32,56]

>>> buildn 12 (L (3,1) ∪ L (4,0))
[0,1,4,7,8,10,12,13,16,19,20,22]

>>> buildn 14 (5⋄4 ∪ 3⋄2 ∪ 7⋄3)
[2,3,4,5,8,9,10,11,14,17,19,20,23,24]

>>> buildn 6 (3⋄0 ∪ 4⋄0)
[0,3,4,6,8,9]

>>> buildn 8 (5⋄2 ∩ 2⋄0 ∪ 7⋄3)
[2,3,10,12,17,22,24,31]

>>> buildn 12 (5⋄1 ∪ 7⋄2)
[1,2,6,9,11,16,21,23,26,30,31,36]

>>> buildn 19 (L (3,2) ∪ L (7, 1))
[1,2,5,8,11,14,15,17,20,22,23,26,29,32,35,36,38,41,43]

>>> buildn 19 (3⋄0 ∪ 7⋄0)
[0,3,6,7,9,12,14,15,18,21,24,27,28,30,33,35,36,39,42]

>>> buildn 10 (3⋄2 ∩ 4⋄7 ∪ 6⋄9 ∩ 15⋄18)
[3,11,23,33,35,47,59,63,71,83]

>>> let s = 3⋄2∩4⋄7∩6⋄11∩8⋄7 ∪ 6⋄9∩15⋄18 ∪ 13⋄5∩8⋄6∩4⋄2 ∪ 6⋄9∩15⋄19
>>> let s' = 24⋄23 ∪ 30⋄3 ∪ 104⋄70
>>> buildn 16 s == buildn 16 s'
True

>>> buildn 10 (24⋄23 ∪ 30⋄3 ∪ 104⋄70)
[3,23,33,47,63,70,71,93,95,119]

>>> buildn 17 (5⋄4 ∪ 3⋄2 ∪ 7⋄3)
[2,3,4,5,8,9,10,11,14,17,19,20,23,24,26,29,31]

>>> buildn 17 (5⋄1 ∪ 3⋄0 ∪ 7⋄3)
[0,1,3,6,9,10,11,12,15,16,17,18,21,24,26,27,30]

>>> buildn 19 (5⋄2 ∪ 3⋄0 ∪ 7⋄4)
[0,2,3,4,6,7,9,11,12,15,17,18,21,22,24,25,27,30,32]

Agon et. al. p.155

>>> let a = c (13⋄3 ∪ 13⋄5 ∪ 13⋄7 ∪ 13⋄9)
>>> let b = 11⋄2
>>> let c' = c (11⋄4 ∪ 11⋄8)
>>> let d = 13⋄9
>>> let e = 13⋄0 ∪ 13⋄1 ∪ 13⋄6
>>> let f = (a ∩ b) ∪ (c' ∩ d) ∪ e
>>> buildn 13 f
[0,1,2,6,9,13,14,19,22,24,26,27,32]

>>> differentiate [0,1,2,6,9,13,14,19,22,24,26,27,32]
[1,1,4,3,4,1,5,3,2,2,1,5]

> import Music.Theory.Pitch
> let n = [0,1,2,6,9,13,14,19,22,24,26,27,32]
> let r = "C C𝄲 C♯ D♯ E𝄲 F𝄰 G A𝄲 B C C♯ C𝄰 E"
> unwords (map (pitch_class_pp . pc24et_to_pitch . (`mod` 24)) n) == r

Jonchaies

>>> let s = map (17⋄) [0,1,4,5,7,11,12,16]
>>> let r = [1,3,1,2,4,1,4,1,1,3,1,2,4,1,4,1,1,3,1,2,4,1,4,1]
>>> differentiate (buildn 25 (union s)) == r
True

> let a2 = octpc_to_midi (2,9)
> let m = scanl (+) a2 r
> import Music.Theory.Pitch.Spelling.Table
> let p = "A2 A#2 C#3 D3 E3 G#3 A3 C#4 D4 D#4 F#4 G4 A4 C#5 D5 F#5 G5 G#5 B5 C6 D6 F#6 G6 B6 C7"
> unwords (map (pitch_pp_iso . midi_to_pitch pc_spell_sharp) m) == p

Nekuïa

>>> let s = [24⋄0,14⋄2,22⋄3,31⋄4,28⋄7,29⋄9,19⋄10,25⋄13,24⋄14,26⋄17,23⋄21,24⋄10,30⋄9,35⋄17,29⋄24,32⋄25,30⋄29,26⋄21,30⋄17,31⋄16]
>>> let r = [2,1,1,3,2,1,3,1,2,1,4,3,1,4,1,4,1,3,1,4,1,3,1,4,1,4,1,1,3,1,3,1,2,3,1,4,1,4,4,1]
>>> differentiate (buildn 41 (union s)) == r
True

> let a0 = octpc_to_midi (0,9)
> let m = scanl (+) a0 r
> import Music.Theory.Pitch.Spelling.Table
> let p = "A0 B0 C1 C#1 E1 F#1 G1 A#1 B1 C#2 D2 F#2 A2 A#2 D3 D#3 G3 G#3 B3 C4 E4 F4 G#4 A4 C#5 D5 F#5 G5 G#5 B5 C6 D#6 E6 F#6 A6 A#6 D7 D#7 G7 B7 C8"
> unwords (map (pitch_pp_iso . midi_to_pitch pc_spell_sharp) m) == p

>>> let s = [8⋄0∩3⋄0,2⋄0∩7⋄2,2⋄1∩11⋄3,31⋄4,4⋄3∩7⋄0,29⋄9,19⋄10,25⋄13,8⋄6∩3⋄2,2⋄1∩13⋄4,23⋄21,8⋄2∩3⋄1,2⋄1∩3⋄0∩5⋄4,5⋄2∩7⋄3,29⋄24,32⋄25,2⋄1∩3⋄2∩5⋄4,2⋄1∩13⋄8,2⋄1∩3⋄2∩5⋄2,31⋄16]
>>> differentiate (buildn 41 (union s)) == r
True

Major scale:

>>> let s = (c(3⋄2) ∩ 4⋄0) ∪ (c(3⋄1) ∩ 4⋄1) ∪ (3⋄2 ∩ 4⋄2) ∪ (c(3⋄0) ∩ 4⋄3)
>>> buildn 7 s
[0,2,4,5,7,9,11]

Nomos Alpha:

>> let s = (c (13⋄3 ∪ 13⋄5 ∪ 13⋄7 ∪ 13⋄9) ∩ 11⋄2) ∪ (c (11⋄4 ∪ 11⋄8) ∩ 13⋄9) ∪ (13⋄0 ∪ 13⋄1 ∪ 13⋄6)
>> buildn 32 s
[0,1,2,6,9,13,14,19,22,24,26,27,32,35,39,40,45,52,53,58,61,65,66,71,78,79,84,87,90,91,92,97]
-}
buildn :: Int -> Sieve -> [Integer]
buildn n = take n . build . reduce

{- | Standard differentiation function.

>>> differentiate [1,3,6,10]
[2,3,4]

>>> differentiate [0,2,4,5,7,9,11,12]
[2,2,1,2,2,2,1]
-}
differentiate :: (Num a) => [a] -> [a]
differentiate x = zipWith (-) (List.tail_err x) x

{- | Euclid's algorithm for computing the greatest common divisor.

>>> euclid 1989 867
51
-}
euclid :: (Integral a) => a -> a -> a
euclid i j =
  let k = i `mod` j
  in if k == 0 then j else euclid j k

{- | Bachet De Méziriac's algorithm.

>>> de_meziriac 15 4
3

>> euclid 15 4
1
-}
de_meziriac :: (Integral a) => a -> a -> a
de_meziriac i j =
  let f t =
        if (t * i) `mod` j /= 1
          then f (t + 1)
          else t
  in if j == 1 then 1 else f 1

{- | Attempt to reduce the 'Intersection' of two 'L' nodes to a singular 'L' node.

>>> reduce_intersection (3,2) (4,7)
Just (12,11)

>>> reduce_intersection (12,11) (6,11)
Just (12,11)

>>> reduce_intersection (12,11) (8,7)
Just (24,23)
-}
reduce_intersection :: (Integral t) => (t, t) -> (t, t) -> Maybe (t, t)
reduce_intersection (m1, i1) (m2, i2) =
  let d = euclid m1 m2
      i1' = i1 `mod` m1
      i2' = i2 `mod` m2
      c1 = m1 `div` d
      c2 = m2 `div` d
      m3 = d * c1 * c2
      t = de_meziriac c1 c2
      i3 = (i1' + t * (i2' - i1') * c1) `mod` m3
  in if d /= 1 && (i1' - i2') `mod` d /= 0
      then Nothing
      else Just (m3, i3)

{- | Reduce the number of nodes at a 'Sieve'.

>>> reduce (L (3,2) ∪ Empty)
L (3,2)

>>> reduce (L (3,2) ∩ Empty)
L (3,2)

>>> reduce (L (3,2) ∩ L (4,7))
L (12,11)

>>> reduce (L (6,9) ∩ L (15,18))
L (30,3)

>>> let s = 3⋄2∩4⋄7∩6⋄11∩8⋄7 ∪ 6⋄9∩15⋄18 ∪ 13⋄5∩8⋄6∩4⋄2 ∪ 6⋄9∩15⋄19
>>> reduce s == (24⋄23 ∪ 30⋄3 ∪ 104⋄70)
True

>>> sieve_pp (reduce s) == "((24.23 ∪ 30.3) ∪ 104.70)"
True

>>> let s = 3⋄2∩4⋄7∩6⋄11∩8⋄7 ∪ 6⋄9∩15⋄18 ∪ 13⋄5∩8⋄6∩4⋄2 ∪ 6⋄9∩15⋄19
>>> reduce s == (24⋄23 ∪ 30⋄3 ∪ 104⋄70)
True
-}
reduce :: Sieve -> Sieve
reduce s =
  let f g s1 s2 =
        let s1' = reduce s1
            s2' = reduce s2
            s' = g s1' s2'
        in if s1 == s1' && s2 == s2'
            then s'
            else reduce s'
  in case s of
      Empty -> Empty
      L _ -> s
      Union s1 Empty -> s1
      Union s1 s2 -> f Union s1 s2
      Intersection s1 Empty -> s1
      Intersection (L p) (L q) -> maybe Empty L (reduce_intersection p q)
      Intersection s1 s2 -> f Intersection s1 s2
      Complement s' -> Complement (reduce s')

-- * Literature

psappha_flint_c :: [Sieve]
psappha_flint_c =
  let s0 = (8 ⋄ 0 ∪ 8 ⋄ 1 ∪ 8 ⋄ 7) ∩ (5 ⋄ 1 ∪ 5 ⋄ 3)
      s1 = (8 ⋄ 0 ∪ 8 ⋄ 1 ∪ 8 ⋄ 2) ∩ 5 ⋄ 0
      s2 = 8 ⋄ 3 ∩ (5 ⋄ 0 ∪ 5 ⋄ 1 ∪ 5 ⋄ 2 ∪ 5 ⋄ 3 ∪ 5 ⋄ 4)
      s3 = 8 ⋄ 4 ∩ (5 ⋄ 0 ∪ 5 ⋄ 1 ∪ 5 ⋄ 2 ∪ 5 ⋄ 3 ∪ 5 ⋄ 4)
      s4 = (8 ⋄ 5 ∪ 8 ⋄ 6) ∩ (5 ⋄ 2 ∪ 5 ⋄ 3 ∪ 5 ⋄ 4)
      s5 = 8 ⋄ 1 ∩ 5 ⋄ 2
      s6 = 8 ⋄ 6 ∩ 5 ⋄ 1
  in [s0, s1, s2, s3, s4, s5, s6]

{- | /Psappha/ (Flint)

>>> buildn 27 psappha_flint
[0,1,3,4,6,8,10,11,12,13,14,16,17,19,20,22,23,25,27,28,29,31,33,35,36,37,38]
-}
psappha_flint :: Sieve
psappha_flint = union psappha_flint_c

a_r_squibbs_c :: [Sieve]
a_r_squibbs_c =
  [ 8 ⋄ 0 ∩ (11 ⋄ 0 ∪ 11 ⋄ 4 ∪ 11 ⋄ 5 ∪ 11 ⋄ 6 ∪ 11 ⋄ 10)
  , 8 ⋄ 1 ∩ (11 ⋄ 2 ∪ 11 ⋄ 3 ∪ 11 ⋄ 6 ∪ 11 ⋄ 7 ∪ 11 ⋄ 9)
  , 8 ⋄ 2 ∩ (11 ⋄ 0 ∪ 11 ⋄ 1 ∪ 11 ⋄ 2 ∪ 11 ⋄ 3 ∪ 11 ⋄ 5 ∪ 11 ⋄ 10)
  , 8 ⋄ 3 ∩ (11 ⋄ 1 ∪ 11 ⋄ 2 ∪ 11 ⋄ 3 ∪ 11 ⋄ 4 ∪ 11 ⋄ 10)
  , 8 ⋄ 4 ∩ (11 ⋄ 0 ∪ 11 ⋄ 4 ∪ 11 ⋄ 8)
  , 8 ⋄ 5 ∩ (11 ⋄ 0 ∪ 11 ⋄ 2 ∪ 11 ⋄ 3 ∪ 11 ⋄ 7 ∪ 11 ⋄ 9 ∪ 11 ⋄ 10)
  , 8 ⋄ 6 ∩ (11 ⋄ 1 ∪ 11 ⋄ 3 ∪ 11 ⋄ 5 ∪ 11 ⋄ 7 ∪ 11 ⋄ 8 ∪ 11 ⋄ 9)
  , 8 ⋄ 7 ∩ (11 ⋄ 1 ∪ 11 ⋄ 3 ∪ 11 ⋄ 6 ∪ 11 ⋄ 7 ∪ 11 ⋄ 8 ∪ 11 ⋄ 10)
  ]

{- | À R. (Hommage à Maurice Ravel) (Squibbs, 1996)

>>> buildn 42 a_r_squibbs
[0,2,3,4,7,9,10,13,14,16,17,21,23,25,29,30,32,34,35,38,39,43,44,47,48,52,53,57,58,59,62,63,66,67,69,72,73,77,78,82,86,87]
-}
a_r_squibbs :: Sieve
a_r_squibbs = union a_r_squibbs_c
