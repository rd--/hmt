-- | Clarence Barlow. \"Two Essays on Theory\".
-- /Computer Music Journal/, 11(1):44-60, 1987.
-- Translated by Henning Lohner.
module Music.Theory.Meter.Barlow_1987 where

import Data.List
import Data.Numbers.Primes {- primes -}
--import Debug.Trace

traceShow :: a -> b -> b
traceShow _ x = x

-- | One indexed variant of 'genericIndex'.
--
-- > map (at [11..13]) [1..3] == [11,12,13]
at :: (Integral n) => [a] -> n -> a
at x i = x `genericIndex` (i - 1)

-- | Variant of 'at' with boundary rules and specified error message.
--
-- > map (at' 'x' [11..13]) [0..4] == [1,11,12,13,1]
-- > at' 'x' [0] 3 == undefined
at' :: (Num a,Integral n,Show m) => m -> [a] -> n -> a
at' m x i =
    let n = genericLength x
    in if i == 0 || i == n + 1
       then 1 -- error (show ("at':==",m,x,i))
       else if i < 0 || i > n + 1
            then error (show ("at'",m,x,i))
            else x `genericIndex` (i - 1)

-- | Variant of 'mod' with input constraints.
--
-- > mod' (-1) 2 == 1
mod' :: Integral a => a -> a -> a
mod' a b =
    let r = mod a b
    in if r < 0 || r >= b
       then error (show ("mod'",a,b,r))
       else r

-- | Alias for 'Double' (quieten compiler).
type R = Double

-- | Specialised variant of 'fromIntegral'.
to_r :: Integral i => i -> R
to_r = fromIntegral

-- | Variant on 'div' with input constraints.
div' :: Integral a => String -> a -> a -> a
div' m i j =
    if i < 0 || j < 0
    then error (show ("div'",m,i,j))
    else truncate (to_r i / to_r j)

-- | A stratification is a tree of integral subdivisions.
type Stratification t = [t]

-- | Indispensibilities from stratification.
--
-- > indispensibilities [3,2,2] == [11,0,6,3,9,1,7,4,10,2,8,5]
-- > indispensibilities [2,3,2] == [11,0,6,2,8,4,10,1,7,3,9,5]
-- > indispensibilities [2,2,3] == [11,0,4,8,2,6,10,1,5,9,3,7]
-- > indispensibilities [3,5] == [14,0,9,3,6,12,1,10,4,7,13,2,11,5,8]
indispensibilities :: Integral a => Stratification a -> [a]
indispensibilities x = map (lower_psi x (genericLength x)) [1 .. product x]

-- | The indispensibility measure (ψ).
--
-- > map (lower_psi [2] 1) [1..2] == [1,0]
-- > map (lower_psi [3] 1) [1..3] == [2,0,1]
-- > map (lower_psi [2,2] 2) [1..4] == [3,0,2,1]
-- > map (lower_psi [5] 1) [1..5] == [4,0,3,1,2]
-- > map (lower_psi [3,2] 2) [1..6] == [5,0,3,1,4,2]
-- > map (lower_psi [2,3] 2) [1..6] == [5,0,2,4,1,3]
lower_psi :: Integral a => Stratification a -> a -> a -> a
lower_psi q z n =
    let s8 r =
            let s1 = product q
                s2 = (n - 2) `mod'` s1
                s3 = let f k = at' "s3" q (z + 1 - k)
                     in product (map f [0 .. r])
                s4 = 1 + div' "s4" s2 s3
                c = at' "c" q (z - r)
                s5 = s4 `mod'` c
                s6 = upper_psi c (1 + s5)
                s7 = let f i = at' "s7" q i
                     in product (map f [0 .. z - r - 1])
            in traceShow ("lower_psi:s",s1,s2,s3,s4,s5,s6,s7) (s7 * s6)
    in traceShow ("lower_psi",q,z,n) (sum (map s8 [0 .. z - 1]))

-- | The first /n/th primes, reversed.
--
-- > reverse_primes 14 == [43,41,37,31,29,23,19,17,13,11,7,5,3,2]
reverse_primes :: Integral n => n -> [n]
reverse_primes n = reverse (genericTake n primes)

-- | Generate prime stratification for /n/.
--
-- > map prime_stratification [2,3,5,7,11] == [[2],[3],[5],[7],[11]]
-- > map prime_stratification [6,8,9,12] == [[3,2],[2,2,2],[3,3],[3,2,2]]
-- > map prime_stratification [22,10,4,1] == [[11,2],[5,2],[2,2],[]]
-- > map prime_stratification [18,16,12] == [[3,3,2],[2,2,2,2],[3,2,2]]
prime_stratification :: Integral n => n -> Stratification n
prime_stratification =
    let go x k =
            case x of
              p:x' -> if k `rem` p == 0
                      then p : go x (div' "ps" k p)
                      else go x' k
              [] -> []
    in go (reverse_primes 14)

-- | Fundamental indispensibilities for prime numbers (Ψ).
--
-- > map (upper_psi 2) [1..2] == [1,0]
-- > map (upper_psi 3) [1..3] == [2,0,1]
-- > map (upper_psi 5) [1..5] == [4,0,3,1,2]
-- > map (upper_psi 7) [1..7] == [6,0,4,2,5,1,3]
upper_psi :: Integral a => a -> a -> a
upper_psi p n =
    if p `notElem` reverse_primes 14
    then error (show ("upper_psi","not prime",p,n))
    else if p == 2
         then p - n
         else if n == p - 1
              then div' "upper_psi" p 4
              else let n' = n - (div' "n'" n p)
                       s = prime_stratification (p - 1)
                       q = lower_psi s (genericLength s) n'
                       q' = to_r q
                       p' = to_r p
                   in truncate (q' + 2 * sqrt ((q' + 1) / p'))

-- | Table such that each subsequent row deletes the least
-- indispensibile pulse.
--
-- > thinning_table [3,2] == [[True,True,True,True,True,True]
-- >                         ,[True,False,True,True,True,True]
-- >                         ,[True,False,True,False,True,True]
-- >                         ,[True,False,True,False,True,False]
-- >                         ,[True,False,False,False,True,False]
-- >                         ,[True,False,False,False,False,False]]
thinning_table :: Integral a => Stratification a -> [[Bool]]
thinning_table s =
    let x = indispensibilities s
        n = genericLength x
        true i = genericReplicate i True
        false i = genericReplicate i False
        f i = true (i + 1)  ++ false (n - i - 1)
    in transpose (map f x)

-- | Trivial pretty printer for 'thinning_table'.
--
-- > putStrLn (thinning_table_pp [3,2])
-- > putStrLn (thinning_table_pp [2,3])
--
-- > ******   ******
-- > *.****   *.****
-- > *.*.**   *.**.*
-- > *.*.*.   *..*.*
-- > *...*.   *..*..
-- > *.....   *.....
thinning_table_pp :: Integral n => Stratification n -> String
thinning_table_pp s =
    let f x = if x then '*' else '.'
    in unlines (map (map f) (thinning_table s))

-- | Scale values against length of list.
--
-- > relative_to_length [0..5] == [0.0,0.2,0.4,0.6,0.8,1.0]
relative_to_length :: (Real a, Fractional b) => [a] -> [b]
relative_to_length x =
    let n = genericLength x - (1::Integer)
    in map ((/ fromIntegral n) . realToFrac) x

-- | Variant of 'indispensibilities' that scales value to lie in
-- @(0,1)@.
--
-- relative_indispensibilities [3,2] == [1,0,0.6,0.2,0.8,0.4]
relative_indispensibilities :: Integral n => Stratification n -> [R]
relative_indispensibilities = relative_to_length . indispensibilities

-- | Align two meters (given as stratifications) to least common
-- multiple of their degrees.  The 'indispensibilities' function is
-- given as an argument so that it may be relative if required.
--
-- > let r = [(5,5),(0,0),(2,3),(4,1),(1,4),(3,2)]
-- > in align_meters indispensibilities [2,3] [3,2] == r
--
-- > let r = [(1,1),(0,0),(0.4,0.6),(0.8,0.2),(0.2,0.8),(0.6,0.4)]
-- > in align_meters relative_indispensibilities [2,3] [3,2] == r
--
-- > align_meters indispensibilities [2,2,3] [3,5]
-- > align_meters relative_indispensibilities [2,2,3] [3,5]
align_meters :: (t -> [b]) -> t -> t -> [(b,b)]
align_meters f s1 s2 =
    let i1 = f s1
        i2 = f s2
        n1 = length i1
        n2 = length i2
        n = lcm n1 n2
        i1' = concat (replicate (n `div` n1) i1)
        i2' = concat (replicate (n `div` n2) i2)
    in zip i1' i2'

-- | Type pairing a stratification and a tempo.
type S_MM t = ([t],t)

-- | Variant of 'div' that requires 'mod' be @0@.
whole_div :: Integral a => a -> a -> a
whole_div i j =
    case i `divMod` j of
      (k,0) -> k
      _ -> error "whole_div"

-- | Variant of 'quot' that requires 'rem' be @0@.
whole_quot :: Integral a => a -> a -> a
whole_quot i j =
    case i `quotRem` j of
      (k,0) -> k
      _ -> error "whole_quot"

-- | Rule to prolong stratification of two 'S_MM' values such that
-- pulse at the deeper level are aligned.
--
-- > let x = ([2,2,2],1)
-- > in prolong_stratifications x x == (fst x,fst x)
--
-- > let r = ([2,5,3,3,2],[3,2,5,5])
-- > in prolong_stratifications ([2,5],50) ([3,2],60) == r
--
-- > prolong_stratifications ([2,2,3],5) ([3,5],4) == ([2,2,3],[3,5])
prolong_stratifications :: Integral a => S_MM a -> S_MM a -> ([a],[a])
prolong_stratifications (s1,v1) (s2,v2) =
    let t1 = product s1 * v1
        t2 = product s2 * v2
        t = lcm t1 t2
        s1' = s1 ++ prime_stratification (t `whole_div` t1)
        s2' = s2 ++ prime_stratification (t `whole_div` t2)
    in (s1',s2')

-- | Arithmetic mean (average) of a list.
--
-- > mean [0..5] == 2.5
mean :: Fractional a => [a] -> a
mean x = sum x / fromIntegral (length x)

-- | Square of /n/.
--
-- > square 5 == 25
square :: Num a => a -> a
square n = n * n

-- | Composition of 'prolong_stratifications' and 'align_meters'.
--
-- > align_s_mm indispensibilities ([2,2,3],5) ([3,5],4)
align_s_mm :: Integral n => ([n] -> [t]) -> S_MM n -> S_MM n -> [(t,t)]
align_s_mm f (s1,v1) (s2,v2) =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
    in align_meters f s1' s2'

{-
-- > map (upper_psi' 2) [1..2] == [1,0]
-- > map (upper_psi' 3) [1..3] == [2,0,1]
-- > map (upper_psi' 5) [1..5] -- == [4,0,3,1,2]
-- > map (upper_psi' 7) [1..7] -- == [6,0,4,2,5,1,3]
upper_psi' :: Integral a => a -> a -> a
upper_psi' h n =
    if h > 3
    then let w x = if x == 0 then 0 else 1
             h4 = div' "h4" h 4
             n' = n - 1 + w (h - n)
             p = prime_stratification (h - 1)
             z = lower_psi p (genericLength p) n'
             a = z + w (div' "z" z h4)
             b = w (h - n - 1)
             c = b + h4 * (1 - b)
         in traceShow ("upper_psi'",h,n) (a * c)
    else (h + n - 2) `mod'` h

mean_square_product :: Fractional n => [(n,n)] -> n
mean_square_product x =
    let f = square . uncurry (*)
        n = square (fromIntegral (length x))
    in sum (map f x) / n

-- metrical_affinity [2,2,2] 1 [2,2,2] 1
-- metrical_affinity [2,3] 1 [3,2] 1
-- metrical_affinity [2,2,3] 20 [3,5] 16
-- metrical_affinity [2,2,2] 1 [2,2,2] 1
-- metrical_affinity [2,2,2] 1 [2,2,3] 1
metrical_affinity s1 v1 s2 v2 =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
        i1 = relative_indispensibilities s1'
        i2 = relative_indispensibilities s2'
        v = lcm v1 v2
        i1' = concat (replicate (v `div` v1) i1)
        i2' = concat (replicate (v `div` v2) i2)
        --p = zipWith (*) (relative_to_length i1') (relative_to_length i2')
        p = zipWith (*) i1' i2'
        square n = n * n
        ps = map square p
        z = length ps
        mps = sum ps / to_r z
    in mps  -- (i1',i2',length i1'==length i2')

-- lcm 12 15 == 60
-- metrical_affinity [2,5] 50 [3,2] 60
-- metrical_affinity [2,2,2] 1 [2,2,2] 1
-- metrical_affinity [2,2,2] 1 [2,2,3] 1
-- metrical_affinity [2,2,2] 1 [2,3,2] 1
--metrical_affinity :: (Integral t) => [t] -> t -> [t] -> t -> (t, t, t, t)
metrical_affinity' s1 v1 s2 v2 =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
        ix f i = if i == 1 then f 1 else if i == 2 then f 2 else undefined
        s = ix (at [s1,s2])
        v = ix (at [v1,v2])
        u = ix (genericLength . s)
        s' = ix (at [s1',s2'])
        z = ix (genericLength . s')
        --z1 = genericLength s1'
        --z2 = genericLength s2'
        q i j = s i `at` j
        --q1 j = s1 `at` j
        --q2 j = s2 `at` j
        omega_u i = product (map (q i) [1 .. u i])
        --omega_u1 = product (map q1 [1 .. u1])
        --omega_u2 = product (map q2 [1 .. u2])
        omega_z _ = lcm (v 1 * omega_u 1) (v 2 * omega_u 2)
        omega_0 = lcm (product (s' 1)) (product (s' 2))
        square n = n * n
        x0 n i = lower_psi (s' i) (z i) (1 + ((n - 1) `mod'` omega_z i))
        x1 n = square (product (map (x0 n) [1,2]))
        x2 = sum (map x1 [1 .. omega_0])
        x3 = 18 * x2 - 2
        x4 i = square (omega_z i - 1)
        x5 = product (map x4 [1,2])
        x6 = 7 * omega_0 * x5
        x7 = to_r x3 / to_r x6
        x8 = 2 * log x7
        x9 = negate (recip x8)
    in (omega_u 1,omega_u 2,omega_z undefined,omega_0
       ,x2,x3,x5,x6,x7,x8,x9)
-}
