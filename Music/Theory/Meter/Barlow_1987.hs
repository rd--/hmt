-- | Clarence Barlow. \"Two Essays on Theory\".
-- /Computer Music Journal/, 11(1):44-60, 1987.
-- Translated by Henning Lohner.
module Music.Theory.Meter.Barlow_1987 where

import Data.List
import Data.Numbers.Primes {- primes -}
--import Debug.Trace

traceShow :: a -> b -> b
traceShow _ x = x

-- | One indexed variant of 'genericIndex' with boundary rules.
--
-- > map (at undefined [11..13]) [1..3] == [11,12,13]
-- > map (at undefined [11..13]) [0..4] == [1,11,12,13,1]
-- > map (at 'x' [11..13]) [-1..5] == [1,11,12,13,1]
at :: (Num a,Integral n,Show m) => m -> [a] -> n -> a
at m x i =
    let n = genericLength x
    in if i == 0 || i == n + 1
       then 1 -- error (show ("at:==",m,x,i))
       else if i < 0 || i > n + 1
            then error (show ("at",m,x,i))
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

type R = Double

to_r :: Integral i => i -> R
to_r = fromIntegral

-- | Variant on 'div' with input constraints.
div' :: Integral a => String -> a -> a -> a
div' m i j =
    if i < 0 || j < 0
    then error (show ("div'",m,i,j))
    else truncate (to_r i / to_r j)

-- | The indispensibility measure (ψ).
--
-- > map (lower_psi [2] 1) [1..2] == [1,0]
-- > map (lower_psi [3] 1) [1..3] == [2,0,1]
-- > map (lower_psi [2,2] 2) [1..4] == [3,0,2,1]
-- > map (lower_psi [5] 1) [1..5] == [4,0,3,1,2]
-- > map (lower_psi [3,2] 2) [1..6] == [5,0,3,1,4,2]
-- > map (lower_psi [2,3] 2) [1..6] == [5,0,2,4,1,3]
-- > map (lower_psi [3,2,2] 3) [1..12] == [11,0,6,3,9,1,7,4,10,2,8,5]
-- > map (lower_psi [2,3,2] 3) [1..12] == [11,0,6,2,8,4,10,1,7,3,9,5]
-- > map (lower_psi [2,2,3] 3) [1..12] == [11,0,4,8,2,6,10,1,5,9,3,7]
-- > map (lower_psi [3,5] 2) [1..15] == [14,0,9,3,6,12,1,10,4,7,13,2,11,5,8]
lower_psi :: Integral a => [a] -> a -> a -> a
lower_psi q z n =
    let s8 r =
            let s1 = product q
                s2 = (n - 2) `mod'` s1
                s3 = let f k = at "s3" q (z + 1 - k)
                     in product (map f [0 .. r])
                s4 = 1 + div' "s4" s2 s3
                c = at "c" q (z - r)
                s5 = s4 `mod'` c
                s6 = upper_psi c (1 + s5)
                s7 = let f i = at "s7" q i
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
prime_stratification :: Integral n => n -> [n]
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

{-
-- > map (upper_psi' 2) [1..2] == [1,0]
-- > map (upper_psi' 3) [1..3] == [2,0,1]
-- > map (upper_psi' 4) [1..4] -- == [4,0,3,3]
-- > map (upper_psi' 5) [1..5] -- == [4,0,3,1,2]
upper_psi' :: Integral a => a -> a -> a
upper_psi' h n =
    if h > 3
    then let w x = if x == 0 then 0 else 1
             n' = n - 1 + w (h - n)
             p = prime_stratification (h - 1)
             z = lower_psi p (genericLength p) n'
             a = z + w (div' "z" z (div' "h4" h 4))
             b = w (h - n - 1)
             c = b + (div' "h4" h 4) * (1 - b)
         in traceShow ("upper_psi",h,n) (a * c)
    else (h + n - 2) `mod'` h
-}
