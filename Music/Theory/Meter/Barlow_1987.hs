{- | Clarence Barlow. \"Two Essays on Theory\".
/Computer Music Journal/, 11(1):44-60, 1987.
Translated by Henning Lohner.
-}
module Music.Theory.Meter.Barlow_1987 where

import Data.List {- base -}
--import Debug.Trace

import qualified Data.Numbers.Primes as P {- primes -}

import qualified Music.Theory.Math as T {- hmt-base -}

traceShow :: a -> b -> b
traceShow _ x = x

{- | One indexed variant of 'genericIndex'.

>>> map (at1 [11..13]) [1..3]
[11,12,13]
-}
at1 :: Integral n => [a] -> n -> a
at1 x i = x `genericIndex` (i - 1)

{- | Variant of 'at1' with boundary rules and specified error message.

>>> map (at1_bnd_err 'x' [11..13]) [0..4]
[1,11,12,13,1]

> at1_bnd_err 'x' [0] 3 == undefined
-}
at1_bnd_err :: (Num a,Show a,Integral n,Show n,Show m) => m -> [a] -> n -> a
at1_bnd_err m x i =
    let n = genericLength x
    in if i == 0 || i == n + 1
       then 1 -- error (show ("at':==",m,x,i))
       else if i < 0 || i > n + 1
            then error (show ("at1_bnd_err",m,x,i))
            else x `genericIndex` (i - 1)

{- | Variant of 'mod' with input constraints.

>>> mod_pos_err (-1) 2
1

> mod_pos_err 1 (-2) == undefined
-}
mod_pos_err :: (Integral a,Show a) => a -> a -> a
mod_pos_err a b =
    let r = mod a b
    in if r < 0 || r >= b
       then error (show ("mod_pos_err",a,b,r))
       else r

{- | Type-specialised variant of 'fromIntegral'. -}
to_r :: Integral n => n -> Double
to_r = fromIntegral

{- | Variant on 'div' with input constraints. -}
div_pos_err :: (Integral a,Show a) => String -> a -> a -> a
div_pos_err m i j =
    if i < 0 || j < 0
    then error (show ("div_pos_err",m,i,j))
    else truncate (to_r i / to_r j)

{- | A stratification is a tree of integral subdivisions. -}
type Stratification t = [t]

{- | Indispensibilities from stratification.

>>> indispensibilities [3,2,2]
[11,0,6,3,9,1,7,4,10,2,8,5]

>>> indispensibilities [2,3,2]
[11,0,6,2,8,4,10,1,7,3,9,5]

>>> indispensibilities [2,2,3]
[11,0,4,8,2,6,10,1,5,9,3,7]

>>> indispensibilities [3,5]
[14,0,9,3,6,12,1,10,4,7,13,2,11,5,8]
-}
indispensibilities :: (Integral n,Show n) => Stratification n -> [n]
indispensibilities x = map (lower_psi x (genericLength x)) [1 .. product x]

{- | The indispensibility measure (ψ).

>>> map (lower_psi [2] 1) [1..2]
[1,0]

>>> map (lower_psi [3] 1) [1..3]
[2,0,1]

>>> map (lower_psi [2,2] 2) [1..4]
[3,0,2,1]

>>> map (lower_psi [5] 1) [1..5]
[4,0,3,1,2]

>>> map (lower_psi [3,2] 2) [1..6]
[5,0,3,1,4,2]

>>> map (lower_psi [2,3] 2) [1..6]
[5,0,2,4,1,3]
-}
lower_psi :: (Integral a,Show a) => Stratification a -> a -> a -> a
lower_psi q z n =
    let s8 r =
            let s1 = product q
                s2 = (n - 2) `mod_pos_err` s1
                s3 = let f k = at1_bnd_err "s3" q (z + 1 - k)
                     in product (map f [0 .. r])
                s4 = 1 + div_pos_err "s4" s2 s3
                c = at1_bnd_err "c" q (z - r)
                s5 = s4 `mod_pos_err` c
                s6 = upper_psi c (1 + s5)
                s7 = let f = at1_bnd_err "s7" q
                     in product (map f [0 .. z - r - 1])
            in traceShow ("lower_psi:s",s1,s2,s3,s4,s5,s6,s7) (s7 * s6)
    in traceShow ("lower_psi",q,z,n) (sum (map s8 [0 .. z - 1]))

{- | The first /n/ primes, reversed.

>>> reverse_primes 14
[43,41,37,31,29,23,19,17,13,11,7,5,3,2]

>>> length (reverse_primes 14)
14
-}
reverse_primes :: Integral n => n -> [n]
reverse_primes n = reverse (genericTake n P.primes)

{- | Generate prime stratification for /n/.

>>> map prime_stratification [2,3,5,7,11]
[[2],[3],[5],[7],[11]]

>>> map prime_stratification [6,8,9,12]
[[3,2],[2,2,2],[3,3],[3,2,2]]

>>> map prime_stratification [22,10,4,1]
[[11,2],[5,2],[2,2],[]]

>>> map prime_stratification [18,16,12]
[[3,3,2],[2,2,2,2],[3,2,2]]
-}
prime_stratification :: (Integral n,Show n) => n -> Stratification n
prime_stratification =
    let go x k =
            case x of
              p:x' -> if k `rem` p == 0
                      then p : go x (div_pos_err "ps" k p)
                      else go x' k
              [] -> []
    in go (reverse_primes 14)

{- | Fundamental indispensibilities for prime numbers (Ψ).

>>> map (upper_psi 2) [1..2]
[1,0]

>>> map (upper_psi 3) [1..3]
[2,0,1]

>>> map (upper_psi 5) [1..5]
[4,0,3,1,2]

>>> map (upper_psi 7) [1..7]
[6,0,4,2,5,1,3]

>>> map (upper_psi 11) [1..11]
[10,0,6,4,9,1,7,3,8,2,5]

>>> map (upper_psi 13) [1..13]
[12,0,7,4,10,1,8,5,11,2,9,3,6]
-}
upper_psi :: (Integral a,Show a) => a -> a -> a
upper_psi p n =
    if p `notElem` reverse_primes 14
    then error (show ("upper_psi","not prime",p,n))
    else if p == 2
         then p - n
         else if n == p - 1
              then div_pos_err "upper_psi" p 4
              else let n' = n - div_pos_err "n'" n p
                       s = prime_stratification (p - 1)
                       q = lower_psi s (genericLength s) n'
                       q' = to_r q
                       p' = to_r p
                   in truncate (q' + 2 * sqrt ((q' + 1) / p'))

{- | Table such that each subsequent row deletes the least indispensibile pulse.

>>> thinning_table [3,2]
[[True,True,True,True,True,True],[True,False,True,True,True,True],[True,False,True,False,True,True],[True,False,True,False,True,False],[True,False,False,False,True,False],[True,False,False,False,False,False]]
-}
thinning_table :: (Integral n,Show n) => Stratification n -> [[Bool]]
thinning_table s =
    let x = indispensibilities s
        n = genericLength x
        true i = genericReplicate i True
        false i = genericReplicate i False
        f i = true (i + 1)  ++ false (n - i - 1)
    in transpose (map f x)

{- | Trivial pretty printer for 'thinning_table'.

> putStrLn (thinning_table_pp [3,2])

> putStrLn (thinning_table_pp [2,3])

@
******   ******
*.****   *.****
*.*.**   *.**.*
*.*.*.   *..*.*
*...*.   *..*..
*.....   *.....
@
-}
thinning_table_pp :: (Integral n,Show n) => Stratification n -> String
thinning_table_pp s =
    let f x = if x then '*' else '.'
    in unlines (map (map f) (thinning_table s))

{- | Scale values against length of list minus one.

>>> relative_to_length [0..5]
[0.0,0.2,0.4,0.6,0.8,1.0]
-}
relative_to_length :: (Real a, Fractional b) => [a] -> [b]
relative_to_length x =
    let n = length x - 1
    in map ((/ fromIntegral n) . realToFrac) x

{- | Variant of 'indispensibilities' that scales value to lie in @(0,1)@.

>>> relative_indispensibilities [3,2]
[1.0,0.0,0.6,0.2,0.8,0.4]
-}
relative_indispensibilities :: (Integral n,Show n) => Stratification n -> [Double]
relative_indispensibilities = relative_to_length . indispensibilities

{- | Align two meters (given as stratifications) to least common multiple of their degrees.
The 'indispensibilities' function is given as an argument so that it may be relative if required.
This generates Table 7 (p.58).
--
>>> align_meters indispensibilities [2,3] [3,2]
[(5,5),(0,0),(2,3),(4,1),(1,4),(3,2)]

>>> align_meters relative_indispensibilities [2,3] [3,2]
[(1.0,1.0),(0.0,0.0),(0.4,0.6),(0.8,0.2),(0.2,0.8),(0.6,0.4)]

>>> align_meters indispensibilities [2,2,3] [3,5]
[(11,14),(0,0),(4,9),(8,3),(2,6),(6,12),(10,1),(1,10),(5,4),(9,7),(3,13),(7,2),(11,11),(0,5),(4,8),(8,14),(2,0),(6,9),(10,3),(1,6),(5,12),(9,1),(3,10),(7,4),(11,7),(0,13),(4,2),(8,11),(2,5),(6,8),(10,14),(1,0),(5,9),(9,3),(3,6),(7,12),(11,1),(0,10),(4,4),(8,7),(2,13),(6,2),(10,11),(1,5),(5,8),(9,14),(3,0),(7,9),(11,3),(0,6),(4,12),(8,1),(2,10),(6,4),(10,7),(1,13),(5,2),(9,11),(3,5),(7,8)]

> align_meters relative_indispensibilities [2,2,3] [3,5]
-}
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

{- | Type pairing a stratification and a tempo. -}
type S_MM t = ([t],t)

{- | Variant of 'div' that requires 'mod_pos_err be @0@. -}
whole_div :: Integral a => a -> a -> a
whole_div i j =
    case i `divMod` j of
      (k,0) -> k
      _ -> error "whole_div"

{- | Variant of 'quot' that requires 'rem' be @0@. -}
whole_quot :: Integral a => a -> a -> a
whole_quot i j =
    case i `quotRem` j of
      (k,0) -> k
      _ -> error "whole_quot"

{- | Rule to prolong stratification of two 'S_MM' values such that pulse at the deeper level are aligned.  (Paragraph 2, p.58)

>>> let x = ([2,2,2],1)
>>> prolong_stratifications x x == (fst x,fst x)
True

>>> prolong_stratifications ([2,5],50) ([3,2],60)
([2,5,3,3,2],[3,2,5,5])

>>> prolong_stratifications ([2,2,3],5) ([3,5],4)
([2,2,3],[3,5])
-}
prolong_stratifications :: (Integral n,Show n) => S_MM n -> S_MM n -> ([n],[n])
prolong_stratifications (s1,v1) (s2,v2) =
    let t1 = product s1 * v1
        t2 = product s2 * v2
        t = lcm t1 t2
        s1' = s1 ++ prime_stratification (t `whole_div` t1)
        s2' = s2 ++ prime_stratification (t `whole_div` t2)
    in (s1',s2')

{- | Composition of 'prolong_stratifications' and 'align_meters'.

>>> align_s_mm indispensibilities ([2,2,3],5) ([3,5],4)
[(11,14),(0,0),(4,9),(8,3),(2,6),(6,12),(10,1),(1,10),(5,4),(9,7),(3,13),(7,2),(11,11),(0,5),(4,8),(8,14),(2,0),(6,9),(10,3),(1,6),(5,12),(9,1),(3,10),(7,4),(11,7),(0,13),(4,2),(8,11),(2,5),(6,8),(10,14),(1,0),(5,9),(9,3),(3,6),(7,12),(11,1),(0,10),(4,4),(8,7),(2,13),(6,2),(10,11),(1,5),(5,8),(9,14),(3,0),(7,9),(11,3),(0,6),(4,12),(8,1),(2,10),(6,4),(10,7),(1,13),(5,2),(9,11),(3,5),(7,8)]
-}
align_s_mm :: (Integral n,Show n) => ([n] -> [t]) -> S_MM n -> S_MM n -> [(t,t)]
align_s_mm f (s1,v1) (s2,v2) =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
    in align_meters f s1' s2'

{- | An attempt at Equation 5 of the /CMJ/ paper.
When /n/ is /h-1/ the output is incorrect (it is the product of the correct values for /n/ at /h-1/ and /h/).

>>> map (upper_psi' 5) [1..5] /= [4,0,3,1,2]
True

>>> map (upper_psi' 7) [1..7] /= [6,0,4,2,5,1,3]
True

>>> map (upper_psi' 11) [1..11] /= [10,0,6,4,9,1,7,3,8,2,5]
True

>>> map (upper_psi' 13) [1..13] /= [12,0,7,4,10,1,8,5,11,2,9,3,6]
True
-}
upper_psi' :: (Integral a,Show a) => a -> a -> a
upper_psi' h n =
    if h > 3
    then let omega x = if x == 0 then 0 else 1
             h4 = div_pos_err "h4" h 4
             n' = n - 1 + omega (h - n)
             p = prime_stratification (h - 1)
             x0 = lower_psi p (genericLength p) n'
             x1 = x0 + omega (div_pos_err "z" x0 h4)
             x2 = omega (h - n - 1)
             x3 = x2 + h4 * (1 - x2)
         in traceShow ("upper_psi'",h,n,n',x0,x1,x2,x3) (x1 * x3)
    else (h + n - 2) `mod_pos_err` h

{- | The /MPS/ limit equation given on p.58.

>>> mps_limit 3 == 21 + 7/9
True
-}
mps_limit :: Floating a => a -> a
mps_limit n = sum [n ** 4 / 9
                  ,n ** 3 / 3
                  ,13 * (n ** 2 ) / 36
                  ,n / 6
                  ,1 / 36]

{- | The square of the product of the input sequence is summed, then divided by the square of the sequence length.

>>> mean_square_product [(0,0),(1,1),(2,2),(3,3)]
6.125
>>> mean_square_product [(2,3),(4,5)] == (6^2 + 20^2) / 2^2
True
-}
mean_square_product :: Fractional n => [(n,n)] -> n
mean_square_product x =
    let f = T.square . uncurry (*)
        n = fromIntegral (length x)
    in sum (map f x) / T.square n

{- | An incorrect attempt at the description in paragraph two of p.58 of the /CMJ/ paper.

>>> let p ~= q = abs (p - q) < 1e-4
>>> metrical_affinity [2,3] 1 [3,2] 1 ~= 0.0324
True

>>> metrical_affinity [2,2,3] 20 [3,5] 16 ~= 0.0028
True
-}
metrical_affinity :: (Integral n,Show n) => [n] -> n -> [n] -> n -> Double
metrical_affinity s1 v1 s2 v2 =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
        i1 = relative_indispensibilities s1'
        i2 = relative_indispensibilities s2'
        v = lcm v1 v2
        i1' = concat (genericReplicate (v `div` v1) i1)
        i2' = concat (genericReplicate (v `div` v2) i2)
    in mean_square_product (zip i1' i2')

{- | An incorrect attempt at Equation 6 of the /CMJ/ paper, see omega_z.

>>> let p ~= q = abs (p - q) < 1e-4
>>> metrical_affinity' [2,2,2] 1 [2,2,2] 1 ~= 1.06735
True

>>> metrical_affinity' [2,2,2] 1 [2,2,3] 1 ~= 0.57185
True

>>> metrical_affinity' [2,2,2] 1 [2,3,2] 1 ~= 0.48575
True

>> metrical_affinity' [2,2,2] 1 [3,2,2] 1 ~= 0.45872
True

>>> metrical_affinity' [3,2,2] 3 [2,2,3] 2 ~= 0.10282
True
-}
metrical_affinity' :: (Integral t,Show t) => [t] -> t -> [t] -> t -> Double
metrical_affinity' s1 v1 s2 v2 =
    let (s1',s2') = prolong_stratifications (s1,v1) (s2,v2)
        ix :: (Integer -> x) -> Integer -> x
        ix f i = case i of
                   1 -> f 1
                   2 -> f 2
                   _ -> error (show ("ix",i))
        s = ix (at1 [s1,s2])
        v = ix (at1 [v1,v2])
        u = ix (genericLength . s)
        s' = ix (at1 [s1',s2'])
        z = ix (genericLength . s')
        q i j = s i `at1` j
        omega_u i = product (map (q i) [1::Int .. u i])
        omega_z _ = lcm (v 1 * omega_u 1) (v 2 * omega_u 2)
        omega_0 = lcm (product (s' 1)) (product (s' 2))
        x0 n i = lower_psi (s' i) (z i) (1 + ((n - 1) `mod_pos_err` omega_z i))
        x1 n = T.square (product (map (x0 n) [1,2]))
        x2 = sum (map x1 [1 .. omega_0])
        x3 = 18 * x2 - 2
        x4 i = T.square (omega_z i - 1)
        x5 = product (map x4 [1::Integer,2])
        x6 = 7 * omega_0 * x5
        x7 = to_r x3 / to_r x6
        x8 = 2 * log x7
        x9 = negate (recip x8)
    in traceShow (omega_z,omega_0,x2,x3,x5,x6,x7,x8,x9) x9
