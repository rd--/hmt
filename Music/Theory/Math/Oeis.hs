-- | The On-Line Encyclopedia of Integer Sequences, <http://oeis.org/>
module Music.Theory.Math.Oeis where

import Data.Bits {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Data.Set as Set {- containers -}

import qualified Data.MemoCombinators as Memo {- data-memocombinators -}

import qualified Music.Theory.Math as Math {- hmt-base -}

import qualified Music.Theory.Math.Prime as Prime {- hmt -}

{- | <http://oeis.org/A000005>

d(n) (also called tau(n) or sigma_0(n)), the number of divisors of n. (Formerly M0246 N0086)

[1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5, 2, 6, 2, 6, 4, 4, 2, 8, 3, 4, 4, 6, 2, 8, 2, 6, 4, 4, 4, 9, 2, 4, 4, 8, 2, 8, 2, 6, 6, 4, 2, 10, 3, 6, 4, 6, 2, 8, 4, 8, 4, 4, 2, 12, 2, 4, 6, 7, 4, 8, 2, 6, 4, 8, 2, 12, 2, 4, 6, 6, 4, 8, 2, 10, 5, 4, 2, 12, 4, 4, 4, 8, 2, 12, 4, 6, 4, 4, 4, 12, 2, 6, 6, 9, 2, 8, 2, 8] `isPrefixOf` a000005
-}
a000005 :: Integral n => [n]
a000005 = map (product . map (+ 1) . a124010_row) [1..]

{- | <http://oeis.org/A000010>

Euler totient function phi(n): count numbers <= n and prime to n.

> [1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8,12,10,22,8,20,12] `isPrefixOf` a000010
-}
a000010 :: Integral n => [n]
a000010 = map a000010_n [1 ..]

a000010_n :: Integral n => n -> n
a000010_n n = genericLength (filter (==1) (map (gcd n) [1..n]))

{- | <http://oeis.org/A000012>

The simplest sequence of positive numbers: the all 1's sequence.
-}
a000012 :: Num n => [n]
a000012 = repeat 1

{- | <https://oeis.org/A000031>

Number of n-bead necklaces with 2 colors when turning over is not allowed; also number of output sequences from a simple n-stage cycling shift register; also number of binary irreducible polynomials whose degree divides n.

> [1,2,3,4,6,8,14,20,36,60,108,188,352,632,1182,2192,4116,7712,14602,27596] `isPrefixOf` a000031
-}
a000031 :: Integral n => [n]
a000031 = map a000031_n [0..]

a000031_n :: Integral n => n -> n
a000031_n n =
  if n == 0
  then 1
  else let divs = a027750_row n
       in ((`div` n) . sum . zipWith (*) (map a000010_n divs) . map (2 ^) . reverse) divs

{- | <http://oeis.org/A000032>

Lucas numbers beginning at 2: L(n) = L(n-1) + L(n-2), L(0) = 2, L(1) = 1. (Formerly M0155)

> [2,1,3,4,7,11,18,29,47,76,123,199,322,521,843,1364,2207,3571,5778,9349,15127] `isPrefixOf` a000032
-}
a000032 :: Num n => [n]
a000032 = 2 : 1 : zipWith (+) a000032 (tail a000032)

{- | <http://oeis.org/A000040>

The prime numbers.

> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103] `isPrefixOf` a000040
-}
a000040 :: Integral n => [n]
a000040 =
  let base = [2, 3, 5, 7, 11, 13, 17]
      larger = p0 : filter prime more
      prime n = all ((> 0) . mod n) (takeWhile (\x -> x*x <= n) larger)
      _ : p0 : more = roll (makeWheels base)
      roll (n,rs) = [n * k + r | k <- [0..], r <- rs]
      makeWheels = foldl nextSize (1,[1])
      nextSize (size,bs) p = (size * p,[r | k <- [0..p-1], b <- bs, let r = size*k+b, mod r p > 0])
  in base ++ larger

{- | <http://oeis.org/A000041>

a(n) is the number of partitions of n (the partition numbers).

[1,1,2,3,5,7,11,15,22,30,42,56,77,101,135,176,231,297,385,490,627,792,1002,1255] `isPrefixOf` a000041
-}
a000041 :: Num n => [n]
a000041 =
  let p_m = Memo.memo2 Memo.integral Memo.integral p
      p _ 0 = 1
      p k m = if m < k then 0 else p_m k (m - k) + p_m (k + 1) m
  in map (p_m 1) [0::Integer ..]

{- | <http://oeis.org/A000045>

Fibonacci numbers

> [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946] `isPrefixOf` a000045
-}
a000045 :: Num n => [n]
a000045 = 0 : 1 : zipWith (+) a000045 (tail a000045)

{- | <http://oeis.org/A000051>

a(n) = 2^n + 1

> [2,3,5,9,17,33,65,129,257,513,1025,2049,4097,8193,16385,32769,65537,131073] `isPrefixOf` a000051
-}
a000051 :: Num n => [n]
a000051 = iterate (subtract 1 . (* 2)) 2

{- | <http://oeis.org/A000071>

a(n) = Fibonacci(n) - 1.

> [0,0,1,2,4,7,12,20,33,54,88,143,232,376,609,986,1596,2583,4180,6764,10945,17710] `isPrefixOf` a000071
-}
a000071 :: Num n => [n]
a000071 = map (subtract 1) (tail a000045)

{- | <http://oeis.org/A000073>

Tribonacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3) for n >= 3 with a(0) = a(1) = 0 and a(2) = 1.

> [0,0,1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136,5768,10609,19513,35890] `isPrefixOf` a000073
-}
a000073 :: Num n => [n]
a000073 = 0 : 0 : 1 : zipWith (+) a000073 (tail (zipWith (+) a000073 (tail a000073)))

{- | <http://oeis.org/A000078>

Tetranacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3) + a(n-4) with a(0)=a(1)=a(2)=0, a(3)=1.

> [0,0,0,1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536,10671,20569,39648] `isPrefixOf` a000078
-}
a000078 :: Num n => [n]
a000078 =
  let f xs = let y = (sum . head . transpose . take 4 . tails) xs in y : f (y:xs)
  in 0 : 0 : 0 : f [0, 0, 0, 1]

{- | <http://oeis.org/A000079>

Powers of 2: a(n) = 2^n. (Formerly M1129 N0432)

> [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536] `isPrefixOf` a000079
> [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536] `isPrefixOf` map (2 ^) [0..]
-}
a000079 :: Num n => [n]
a000079 = iterate (* 2) 1

{- | <http://oeis.org/A000085>

Number of self-inverse permutations on n letters, also known as involutions; number of standard Young tableaux with n cells.

> [1,1,2,4,10,26,76,232,764,2620,9496,35696,140152,568504,2390480,10349536] `isPrefixOf` a000085
-}
a000085 :: Integral n => [n]
a000085 = 1 : 1 : zipWith (+) (zipWith (*) [1..] a000085) (tail a000085)

{- | <http://oeis.org/A000108>

Catalan numbers: C(n) = binomial(2n,n)/(n+1) = (2n)!/(n!(n+1)!).

> [1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845] `isPrefixOf` a000108
-}
a000108 :: Num n => [n]
a000108 = map last (iterate (scanl1 (+) . (++ [0])) [1])

{- | <http://oeis.org/A000120>

1's-counting sequence: number of 1's in binary expansion of n (or the binary weight of n).

> [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3] `isPrefixOf` a000120
-}
a000120 :: Integral i => [i]
a000120 = let r = [0] : (map . map) (+ 1) (scanl1 (++) r) in concat r

{- | <http://oeis.org/A000142>

Factorial numbers: n! = 1*2*3*4*...*n
(order of symmetric group S_n, number of permutations of n letters).

> [1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800] `isPrefixOf` a000142
-}
a000142 :: (Enum n, Num n) => [n]
a000142 = 1 : zipWith (*) [1..] a000142

{- | https://oeis.org/A000201

Lower Wythoff sequence (a Beatty sequence): a(n) = floor(n*phi), where phi = (1+sqrt(5))/2 = A001622

> [1,3,4,6,8,9,11,12,14,16,17,19,21,22,24,25,27,29,30,32,33,35,37,38,40,42] `isPrefixOf` a000201

> import Sound.Sc3.Plot {- hsc3-plot -}
> plot_p1_imp [take 128 a000201 :: [Int]]
-}
a000201 :: Integral n => [n]
a000201 =
  let f (x:xs) (y:ys) = y : f xs (delete (x + y) ys)
      f _ _ = error "a000201"
  in f [1..] [1..]

{- | <https://oeis.org/A000204>

Lucas numbers (beginning with 1): L(n) = L(n-1) + L(n-2) with L(1) = 1, L(2) = 3

> [1,3,4,7,11,18,29,47,76,123,199,322,521,843,1364,2207,3571,5778,9349,15127] `isPrefixOf` a000204
-}
a000204 :: Num n => [n]
a000204 = 1 : 3 : zipWith (+) a000204 (tail a000204)

{- | <http://oeis.org/A000213>

Tribonacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3) with a(0)=a(1)=a(2)=1.

[1,1,1,3,5,9,17,31,57,105,193,355,653,1201,2209,4063,7473,13745,25281,46499]  `isPrefixOf` a000213
-}
a000213 :: Num n => [n]
a000213 = 1 : 1 : 1 : zipWith (+) a000213 (tail (zipWith (+) a000213 (tail a000213)))

{- | <https://oeis.org/A000217>

Triangular numbers: a(n) = binomial(n+1,2) = n(n+1)/2 = 0 + 1 + 2 + ... + n.

> [0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,253,276] `isPrefixOf` a000217
-}
a000217 :: (Enum n,Num n) => [n]
a000217 = scanl1 (+) [0..]

{- | <http://oeis.org/A000225>

a(n) = 2^n - 1 (Sometimes called Mersenne numbers, although that name is usually reserved for A001348)

> [0,1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535] `isPrefixOf` a000225
-}
a000225 :: Num n => [n]
a000225 = iterate ((+ 1) . (* 2)) 0

{- | <http://oeis.org/000285>

a(0) = 1, a(1) = 4, and a(n) = a(n-1) + a(n-2) for n >= 2. (Formerly M3246 N1309)

> [1,4,5,9,14,23,37,60,97,157,254,411,665,1076,1741,2817,4558,7375,11933,19308] `isPrefixOf` a000285
-}
a000285 :: Num n => [n]
a000285 = 1 : 4 : zipWith (+) a000285 (tail a000285)

{- | <http://oeis.org/A000290>

The squares of the non-negative integers.

> [0,1,4,9,16,25,36,49,64,81,100] `isPrefixOf` a000290
-}
a000290 :: Integral n => [n]
a000290 = let square n = n * n in map square [0..]

{- | <https://oeis.org/A000292>

Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6.

> [0,1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140,1330,1540] `isPrefixOf` a000292
-}
a000292 :: (Enum n,Num n) => [n]
a000292 = scanl1 (+) a000217

{- | <http://oeis.org/A000384>

Hexagonal numbers: a(n) = n*(2*n-1). (Formerly M4108 N1705)

> [0,1,6,15,28,45,66,91,120,153,190,231,276,325,378,435,496,561,630,703,780,861] `isPrefixOf` a000384
-}
a000384 :: Integral n => [n]
a000384 = scanl (+) 0 a016813

{- | <http://oeis.org/A000578>

The cubes: a(n) = n^3.

> [0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744,3375,4096,4913,5832] `isPrefixOf` a000578
-}
a000578 :: Num n => [n]
a000578 =
  0 : 1 : 8 :
  zipWith (+) (map (+ 6) a000578) (map (* 3) (tail (zipWith (-) (tail a000578) a000578)))

{- | <http://oeis.org/A000583>

Fourth powers: a(n) = n^4.

> [0,1,16,81,256,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,50625] `isPrefixOf` a000583
-}
a000583 :: Integral n => [n]
a000583 = scanl (+) 0 a005917

{- | <http://oeis.org/A000670>

Fubini numbers: number of preferential arrangements of n labeled elements; or number of weak orders on n labeled elements; or number of ordered partitions of [n].

> [1,1,3,13,75,541,4683,47293,545835,7087261,102247563,1622632573,28091567595] `isPrefixOf` a000670
-}
a000670 :: Integral n => [n]
a000670 =
  let f xs (bs:bss) = let y = sum (zipWith (*) xs bs) in y : f (y : xs) bss
      f _ _ = error "a000670d"
  in 1 : f [1] (map tail (tail a007318_tbl))

{- | <https://oeis.org/A000796>

Decimal expansion of Pi (or digits of Pi).

> [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,2,7,9,5,0,2,8,8,4,1,9] `isPrefixOf` a000796

> pi :: Data.Number.Fixed.Fixed Data.Number.Fixed.Prec500 {- numbers -}
-}
a000796 :: Integral n => [n]
a000796 =
  let gen _ [] = error "A000796"
      gen z (x:xs) =
        let lb = approx z 3
            approx (a,b,c) n = div (a * n + b) c
            mult (a,b,c) (d,e,f) = (a * d,a * e + b * f,c * f)
        in if lb /= approx z 4
           then gen (mult z x) xs
        else lb : gen (mult (10,-10 * lb,1) z) (x:xs)
  in map fromInteger (gen (1,0,1) [(n,a*d,d) | (n,d,a) <- map (\k -> (k,2 * k + 1,2)) [1..]])

{- | <https://oeis.org/A000930>

Narayana's cows sequence.

> [1,1,1,2,3,4,6,9,13,19,28,41,60] `isPrefixOf` a000930
-}
a000930 :: Num n => [n]
a000930 = 1 : 1 : 1 : zipWith (+) a000930 (drop 2 a000930)

{- | <https://oeis.org/A000931>

Padovan sequence (or Padovan numbers): a(n) = a(n-2) + a(n-3) with a(0) = 1, a(1) = a(2) = 0.

> [1,0,0,1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265] `isPrefixOf` a000931
-}
a000931 :: Num n => [n]
a000931 = 1 : 0 : 0 : zipWith (+) a000931 (tail a000931)

{- | <https://oeis.org/A001008>

Numerators of harmonic numbers H(n) = Sum_{i=1..n} 1/i

[1,3,11,25,137,49,363,761,7129,7381,83711,86021,1145993,1171733,1195757,2436559] `isPrefixOf` a001008
-}
a001008 :: Integral i => [i]
a001008 = map numerator (scanl1 (+) (map (1 %) [1..]))

{- | <http://oeis.org/A001037>

Number of degree-n irreducible polynomials over GF(2); number of
n-bead necklaces with beads of 2 colors when turning over is not
allowed and with primitive period n; number of binary Lyndon words of
length n.

> [1,2,1,2,3,6,9,18,30,56,99,186,335,630,1161,2182,4080,7710,14532,27594,52377,99858,190557,364722,698870] `isPrefixOf` a001037
-}
a001037 :: Integral n => [n]
a001037 = map a001037_n [0..]

a001037_n :: Integral n => n -> n
a001037_n n = if n == 0 then 1 else (sum (map (\d -> (2 ^ d) * a008683_n (n `div` d)) (a027750_row n))) `div` n

{- | <http://oeis.org/A001113>

Decimal expansion of e.

> [2,7,1,8,2,8,1,8,2,8,4,5,9,0,4,5,2,3,5,3,6,0,2,8,7,4,7,1,3,5,2,6,6,2,4,9,7,7,5] `isPrefixOf` a001113

> exp 1 :: Data.Number.Fixed.Fixed Data.Number.Fixed.Prec500 {- numbers -}
-}
a001113 :: Integral n => [n]
a001113 =
  let gen _ [] = error "A001113"
      gen z (x:xs) =
        let lb = approx z 1
            approx (a,b,c) n = div (a * n + b) c
            mult (a,b,c) (d,e,f) = (a * d,a * e + b * f,c * f)
        in if lb /= approx z 2
           then gen (mult z x) xs
           else lb : gen (mult (10,-10 * lb,1) z) (x:xs)
  in gen (1,0,1) [(n,a * d,d) | (n,d,a) <- map (\k -> (1,k,1)) [1..]]

{- | <https://oeis.org/A001147>

Double factorial of odd numbers: a(n) = (2*n-1)!! = 1*3*5*...*(2*n-1). (Formerly M3002 N1217)

> [1,1,3,15,105,945,10395,135135,2027025,34459425,654729075,13749310575] `isPrefixOf` a001147
-}
a001147 :: Integral t => [t]
a001147 = 1 : zipWith (*) [1, 3 ..] a001147

{- | <https://oeis.org/A001156>

Number of partitions of n into squares.

> [1,1,1,1,2,2,2,2,3,4,4,4,5,6,6,6,8,9,10,10,12,13,14,14,16,19,20,21,23,26,27,28] `isPrefixOf` a001156
-}
a001156 :: Num n => [n]
a001156 =
  let p _ 0 = 1
      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m
      p _ _ = error "A001156"
  in map (p (tail a000290)) [0::Integer ..]

{- | <https://oeis.org/A001333>

Numerators of continued fraction convergents to sqrt(2).

[1,1,3,7,17,41,99,239,577,1393,3363,8119,19601,47321,114243,275807,665857] `isPrefixOf` a001333
-}
a001333 :: Num n => [n]
a001333 = 1 : 1 : zipWith (+) a001333 (map (* 2) (tail a001333))

{- | <http://oeis.org/A001622>

Decimal expansion of golden ratio phi (or tau) = (1 + sqrt(5))/2.

> [1,6,1,8,0,3,3,9,8,8,7,4,9,8,9,4,8,4,8,2,0,4,5,8,6,8,3,4,3,6,5,6,3,8,1,1,7,7,2] `isPrefixOf` a001622

> a001622_k :: Data.Number.Fixed.Fixed Data.Number.Fixed.Prec500 {- numbers -}
-}
a001622 :: Num n => [n]
a001622 = map (fromIntegral . digitToInt) "161803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475408807538689175212663386222353693179318006076672635443338908659593958290563832266131992829026788067520876689250171169620703222104321626954862629631361443814975870122034080588795445474924618569536486444924104432077134494704956584678850987433944221254487706647809158846074998871240076521705751797883416625624940758906970400028121042762177111777805315317141011704666599146697987317613560067087480711" ++ error "A001622"

a001622_k :: Floating n => n
a001622_k = (1 + sqrt 5) / 2

{- |  <http://oeis.org/A001644>

a(n) = a(n-1) + a(n-2) + a(n-3), a(0)=3, a(1)=1, a(2)=3.

[3,1,3,7,11,21,39,71,131,241,443,815,1499,2757,5071,9327,17155,31553,58035,106743] `isPrefixOf` a001644
-}
a001644 :: Num n => [n]
a001644 = 3 : 1 : 3 : zipWith3 (((+) .) . (+)) a001644 (tail a001644) (drop 2 a001644)

{- | <https://oeis.org/A001653>

Numbers k such that 2*k^2 - 1 is a square.

> [1, 5, 29, 169, 985, 5741, 33461, 195025, 1136689, 6625109, 38613965, 225058681, 1311738121, 7645370045, 44560482149] `isPrefixOf` a001653

-}
a001653 :: [Integer]
a001653 = 1 : 5 : zipWith (-) (map (* 6) (tail a001653)) a001653

{- | <http://oeis.org/A001687>

a(n) = a(n-2) + a(n-5).

[0,1,0,1,0,1,1,1,2,1,3,2,4,4,5,7,7,11,11,16,18,23,29,34,45,52,68,81,102,126,154] `isPrefixOf` a001687
-}
a001687 :: Num n => [n]
a001687 = 0 : 1 : 0 : 1 : 0 : zipWith (+) a001687 (drop 3 a001687)

{- | <https://oeis.org/A001844>

Centered square numbers: a(n) = 2*n*(n+1)+1. Sums of two consecutive squares. Also, consider all Pythagorean triples (X, Y, Z=Y+1) ordered by increasing Z; then sequence gives Z values.

> [1,5,13,25,41,61,85,113,145,181,221,265,313,365,421,481,545,613,685,761,841,925,1013,1105,1201,1301] `isPrefixOf` a001844
> let k = 999 in take k a001844 == zipWith (+) (take k a000290) (tail a000290)
-}
a001844 :: Integral n => [n]
a001844 = map (\n -> 2 * n * (n + 1) + 1) [0..]

{- | <https://oeis.org/A001950>

Upper Wythoff sequence (a Beatty sequence): a(n) = floor(n*phi^2), where phi = (1+sqrt(5))/2

> [2,5,7,10,13,15,18,20,23,26,28,31,34,36,39,41,44,47,49,52,54,57,60,62,65] `isPrefixOf` a001950
-}
a001950 :: Integral n => [n]
a001950 = zipWith (+) a000201 [1..]

-- | <http://oeis.org/A002267>
--
-- The 15 supersingular primes.
a002267 :: Num n => [n]
a002267 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 41, 47, 59, 71]

{- | <https://oeis.org/A002487>

Stern's diatomic series (or Stern-Brocot sequence)

> [0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5] `isPrefixOf` a002487
-}
a002487 :: Num n => [n]
a002487 =
  let f (a:a') (b:b') = a + b : a : f a' b'
      f _ _ = error "a002487"
      x = 1 : 1 : f (tail x) x
  in 0 : x

{- | <https://oeis.org/A002858>

Ulam numbers: a(1) = 1; a(2) = 2; for n>2, a(n) = least number > a(n-1) which is a unique sum of two distinct earlier terms.

> [1, 2, 3, 4, 6, 8, 11, 13, 16, 18, 26, 28, 36, 38, 47, 48, 53, 57, 62, 69, 72, 77, 82, 87, 97, 99, 102, 106, 114, 126] `isPrefixOf` a002858
-}
a002858 :: [Integer]
a002858 = 1 : 2 : ulam 2 2 a002858

ulam :: Int -> Integer -> [Integer] -> [Integer]
ulam n u us =
  let u' = f (0 :: Integer) (u + 1) us'
      f 2 z _                         = f 0 (z + 1) us'
      f e z (v:vs) | z - v <= v       = if e == 1 then z else f 0 (z + 1) us'
                   | z - v `elem` us' = f (e + 1) z vs
                   | otherwise        = f e z vs
      f _ _ []                        = error "ulam?"
      us' = take n us
  in u' : ulam (n + 1) u' us

{- | <http://oeis.org/A003108>

Number of partitions of n into cubes.

> [1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,5,5,5,5,5,6,6,6,7,7,7,7] `isPrefixOf` a003108
-}
a003108 :: Num n => [n]
a003108 =
  let p _ 0 = 1
      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m
      p _ _ = error "A003108"
  in map (p (tail a000578)) [0::Integer ..]

a003215_n :: Num n => n -> n
a003215_n n = 3 * n * (n + 1) + 1

{- | <http://oeis.org/A003215>

Hex (or centered hexagonal) numbers: 3*n*(n+1)+1 (crystal ball sequence for hexagonal lattice).

> [1,7,19,37,61,91,127,169,217,271,331,397,469,547,631,721,817,919,1027,1141] `isPrefixOf` a003215
-}
a003215 :: (Enum n,Num n) => [n]
a003215 = map a003215_n [0..]

-- | <http://oeis.org/A003269>
--
-- > [0,1,1,1,1,2,3,4,5,7,10,14,19,26,36,50,69,95,131,181,250,345,476,657] `isPrefixOf` a003269
a003269 :: Num n => [n]
a003269 = 0 : 1 : 1 : 1 : zipWith (+) a003269 (drop 3 a003269)

{- | <http://oeis.org/A003520>

a(n) = a(n-1) + a(n-5); a(0) = ... = a(4) = 1.

> [1,1,1,1,1,2,3,4,5,6,8,11,15,20,26,34,45,60,80,106,140,185,245,325,431] `isPrefixOf` a003520
-}
a003520 :: Num n => [n]
a003520 = 1 : 1 : 1 : 1 : 1 : zipWith (+) a003520 (drop 4 a003520)

{- | <http://oeis.org/A003462>

a(n) = (3^n - 1)/2. (Formerly M3463)

[0, 1, 4, 13, 40, 121, 364, 1093, 3280, 9841, 29524, 88573, 265720, 797161, 2391484, 7174453] `isPrefixOf` a003462
-}
a003462 :: [Integer]
a003462 = iterate ((+ 1) . (* 3)) 0

a003462_n :: Integer -> Integer
a003462_n = (`div` 2) . (subtract 1) . (3 ^)

{- | <http://oeis.org/A003586>

3-smooth numbers: numbers of the form 2^i*3^j with i, j >= 0

[1, 2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 27, 32, 36, 48, 54, 64, 72, 81, 96, 108, 128, 144, 162] `isPrefixOf` a003586
-}
a003586 :: [Integer]
a003586 =
  let smooth s = let (x, s') = Set.deleteFindMin s in x : smooth (Set.insert (3 * x) (Set.insert (2 * x) s'))
  in  smooth (Set.singleton 1)

{- | <https://oeis.org/A003849>

The infinite Fibonacci word (start with 0, apply 0->01, 1->0, take limit).

> [0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0] `isPrefixOf` a003849
-}
a003849 :: Num n => [n]
a003849 =
  let fws = [1] : [0] : zipWith (++) fws (tail fws)
  in tail (concat fws)

{- | <http://oeis.org/A004001>

Hofstadter-Conway sequence: a(n) = a(a(n-1)) + a(n-a(n-1)) with a(1) = a(2) = 1.

> [1,1,2,2,3,4,4,4,5,6,7,7,8,8,8,8,9,10,11,12,12,13,14,14,15,15,15,16,16,16,16,16] `isPrefixOf` a004001

> plot_p1_ln [take 250 a004001]
> plot_p1_ln [zipWith (-) a004001 (map (`div` 2) [1 .. 2000])]

-}
a004001 :: [Int]
a004001 =
  let h n x =
        let x' = a004001 !! (x - 1) + a004001 !! (n - x - 1)
        in x' : h (n + 1) x'
  in 1 : 1 : h 3 1

{- | <http://oeis.org/A004718>

Per Nørgård's "infinity sequence"

> take 32 a004718 == [0,1,-1,2,1,0,-2,3,-1,2,0,1,2,-1,-3,4,1,0,-2,3,0,1,-1,2,-2,3,1,0,3,-2,-4,5]

> plot_p1_imp [take 1024 a004718]

<https://www.tandfonline.com/doi/abs/10.1080/17459737.2017.1299807>
<https://arxiv.org/pdf/1402.3091.pdf>

-}
a004718 :: Num n => [n]
a004718 = 0 : concat (transpose [map (+ 1) a004718, map negate (tail a004718)])

{- | <http://oeis.org/A005185>

Hofstadter Q-sequence: a(1) = a(2) = 1; a(n) = a(n-a(n-1)) + a(n-a(n-2)) for n > 2.

> [1,1,2,3,3,4,5,5,6,6,6,8,8,8,10,9,10,11,11,12,12,12,12,16,14,14,16,16,16,16,20] `isPrefixOf` a005185
-}
a005185 :: [Int]
a005185 =
  let ix n = a005185 !! (n - 1)
      zadd = zipWith (+)
      zsub = zipWith (-)
  in 1 : 1 : zadd (map ix (zsub [3..] a005185)) (map ix (zsub [3..] (tail a005185)))

{- | <https://oeis.org/A005448>

Centered triangular numbers: a(n) = 3n(n-1)/2 + 1.

> [1,4,10,19,31,46,64,85,109,136,166,199,235,274,316,361,409,460,514,571,631,694] `isPrefixOf` a005448

> map a005448_n [1 .. 1000] `isPrefixOf` a005448
-}
a005448 :: Integral n => [n]
a005448 = 1 : zipWith (+) a005448 [3,6 ..]

a005448_n :: Integral n => n -> n
a005448_n n = 3 * n * (n - 1) `div` 2 + 1

{- | <http://oeis.org/A005728>

Number of fractions in Farey series of order n.

> [1,2,3,5,7,11,13,19,23,29,33,43,47,59,65,73,81,97,103,121,129,141,151] `isPrefixOf` a005728
-}
a005728 :: Integral i => [i]
a005728 =
  let phi n = genericLength (filter (==1) (map (gcd n) [1..n]))
      f n = if n == 0 then 1 else f (n - 1) + phi n
  in map f [0::Integer ..]

{- | <http://oeis.org/A005811>

Number of runs in binary expansion of n (n>0); number of 1's in Gray code for n

> take 32 a005811 == [0,1,2,1,2,3,2,1,2,3,4,3,2,3,2,1,2,3,4,3,4,5,4,3,2,3,4,3,2,3,2,1]
-}
a005811 :: Integral n => [n]
a005811 =
  let f (x:xs) = x : f (xs ++ [x + x `mod` 2, x + 1 - x `mod` 2])
      f _ = error "A005811"
  in 0 : f [1]

{- | <http://oeis.org/A005917>

Rhombic dodecahedral numbers: a(n) = n^4 - (n - 1)^4.

> [1,15,65,175,369,671,1105,1695,2465,3439,4641,6095,7825,9855,12209,14911,17985] `isPrefixOf` a005917
-}
a005917 :: Integral n => [n]
a005917 =
  let f x ws = let (us,vs) = splitAt x ws in us : f (x + 2) vs
  in map sum (f 1 [1, 3 ..])

{- | <https://oeis.org/A006003>

a(n) = n*(n^2 + 1)/2.

> [0,1,5,15,34,65,111,175,260,369,505,671,870,1105,1379,1695,2056,2465,2925,3439] `isPrefixOf` a006003

> map a006003_n [0 .. 1000] `isPrefixOf` a006003
-}
a006003 :: Integral n => [n]
a006003 = scanl (+) 0 a005448

a006003_n :: Integral n => n -> n
a006003_n n = n * (n ^ (2::Int) + 1) `div` 2

{- | <http://oeis.org/A006046>

Total number of odd entries in first n rows of Pascal's triangle: a(0) = 0, a(1) = 1, a(2k) = 3*a(k), a(2k+1) = 2*a(k) + a(k+1).

> [0,1,3,5,9,11,15,19,27,29,33,37,45,49,57,65,81,83,87,91,99,103,111,119,135,139] `isPrefixOf` a006046

> import Sound.SC3.Plot {- hsc3-plot -}
> plot_p1_ln [take 250 a006046]
> let t = log 3 / log 2
> plot_p1_ln [zipWith (/) (map fromIntegral a006046) (map (\n -> n ** t) [0.0,1 .. 200])]
-}
a006046 :: [Int]
a006046 = map (sum . concat) (inits a047999_tbl)

{- | <http://oeis.org/A006052>

Number of magic squares of order n composed of the numbers from 1 to n^2, counted up to rotations and reflections.

> [1,0,1,880,275305224] == a006052
-}
a006052 :: Integral n => [n]
a006052 = [1,0,1,880,275305224]

{- | <http://oeis.org/A006368>

The "amusical permutation" of the nonnegative numbers: a(2n)=3n, a(4n+1)=3n+1, a(4n-1)=3n-1.

> [0,1,3,2,6,4,9,5,12,7,15,8,18,10,21,11,24,13,27,14,30,16,33,17,36,19,39,20,42,22,45,23,48,25] `isPrefixOf` a006368
> plot_p1_ln [take 100 (a006368 :: [Int])]
> plot_p1_pt [take 2000 (a006368 :: [Int])]
-}
a006368 :: Integral n => [n]
a006368 =
  let f n | u' == 0   = 3 * u
          | otherwise = 3 * v + (v' + 1) `div` 2
        where (u, u') = divMod n 2; (v, v') = divMod n 4
  in map f [0..]

-- Reinhard Zumkeller, Apr 18 2012

{- | <http://oeis.org/A006842>

Triangle read by rows: row n gives numerators of Farey series of order n.

> [0,1,0,1,1,0,1,1,2,1,0,1,1,1,2,3,1,0,1,1,1,2,1,3,2,3,4,1,0,1,1,1,1,2,1,3] `isPrefixOf` a006842
> plot_p1_imp [take 200 (a006842 :: [Int])]
> plot_p1_pt [take 10000 (a006842 :: [Int])]
-}
a006842 :: Integral i => [i]
a006842 = map numerator (concatMap Math.farey [1..])

{- | <http://oeis.org/A006843>

Triangle read by rows: row n gives denominators of Farey series of order n

> [1,1,1,2,1,1,3,2,3,1,1,4,3,2,3,4,1,1,5,4,3,5,2,5,3,4,5,1,1,6,5,4,3,5,2,5] `isPrefixOf` a006843
> plot_p1_imp [take 200 (a006843 :: [Int])]
> plot_p1_pt [take 10000 (a006843 :: [Int])]
-}
a006843 :: Integral i => [i]
a006843 = map denominator (concatMap Math.farey [1..])

{- | <https://oeis.org/A007318>

Pascal's triangle read by rows

[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]] `isPrefixOf` a007318_tbl
-}
a007318 :: Integral i => [i]
a007318 = concat a007318_tbl

a007318_tbl :: Integral i => [[i]]
a007318_tbl =
  let f r = zipWith (+) (0 : r) (r ++ [0])
  in iterate f [1]

{- | <https://oeis.org/A008277>

Triangle of Stirling numbers of the second kind, S2(n,k), n >= 1, 1 <= k <= n.

[1,1,1,1,3,1,1,7,6,1,1,15,25,10,1,1,31,90,65,15,1,1,63,301,350,140,21,1] `isPrefixOf` a008277
-}
a008277 :: (Enum n,Num n) => [n]
a008277 = concat a008277_tbl

a008277_tbl :: (Enum n,Num n) => [[n]]
a008277_tbl = map tail a048993_tbl

{- | <http://oeis.org/A008278>

Triangle of Stirling numbers of 2nd kind, S(n,n-k+1), n >= 1, 1<=k<=n.

[1,1,1,1,3,1,1,6,7,1,1,10,25,15,1,1,15,65,90,31,1,1,21,140,350,301,63,1] `isPrefixOf` a008278
-}
a008278 :: (Enum n,Num n) => [n]
a008278 = concat a008278_tbl

a008278_tbl :: (Enum n,Num n) => [[n]]
a008278_tbl =
  let f p =
        let q = reverse (zipWith (*) [1..] (reverse p))
        in zipWith (+) (0 : q) (p ++ [0])
  in iterate f [1]

{- | <http://oeis.org/A008683>

Möbius (or Moebius) function mu(n). mu(1) = 1; mu(n) = (-1)^k if n is the product of k different primes; otherwise mu(n) = 0.

> [1,-1,-1,0,-1,1,-1,0,0,1,-1,0,-1,1,1,0,-1,0,-1,0,1,1,-1,0,0,1,0,0,-1,-1,-1,0,1] `isPrefixOf` a008683
-}
a008683 :: Integral n => [n]
a008683 = map a008683_n [1..]

a008683_n :: Integral n => n -> n
a008683_n =
  let mu [] = 1
      mu (1:es) = - mu es
      mu _ = 0
  in mu . snd . unzip . Prime.prime_factors_m 

{- | <http://oeis.org/A010049>

Second-order Fibonacci numbers.

> [0,1,1,3,5,10,18,33,59,105,185,324,564,977,1685,2895,4957,8462,14406,24465,41455] `isInfixOf` a010049
-}
a010049 :: Num n => [n]
a010049 =
  let c us (v:vs) = sum (zipWith (*) us (1 : reverse us)) : c (v:us) vs
      c _ _ = error "A010049"
  in uncurry c (splitAt 1 a000045)

{- | <https://oeis.org/A010060>

Thue-Morse sequence: let A_k denote the first 2^k terms; then A_0 = 0 and for k >= 0, A_{k+1} = A_k B_k, where B_k is obtained from A_k by interchanging 0's and 1's.

[0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0] `isPrefixOf` a010060

-}
a010060 :: [Integer]
a010060 =
  let interleave (x:xs) ys = x : interleave ys xs
      interleave [] _ = error "a010060?"
   in 0 : interleave (map (1 -) a010060) (tail a010060)

{- | <https://oeis.org/A014081>

a(n) is the number of occurrences of '11' in binary expansion of n.

> [0, 0, 0, 1, 0, 0, 1, 2, 0, 0, 0, 1, 1, 1, 2, 3, 0, 0, 0, 1, 0, 0, 1, 2, 1, 1, 1, 2, 2, 2, 3, 4, 0, 0, 0, 1, 0, 0, 1, 2] `isPrefixOf` a014081

-}
a014081 :: (Integral i, Bits i) => [i]
a014081 = map (\n -> a000120 !! (n .&. div n 2)) [0..]

{- | <https://oeis.org/A014577>

The regular paper-folding sequence (or dragon curve sequence).

> [1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1] `isPrefixOf` a014577
-}
a014577 :: Integral i => [i]
a014577 =
  let f n = if n `rem` 2 == 1 then f (n `quot` 2) else 1 - (n `div` 2 `rem` 2)
  in map f [0..]

{- | <http://oeis.org/A016813>

a(n) = 4*n + 1.

> [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97,101] `isPrefixOf` a016813
-}
a016813 :: Integral n => [n]
a016813 = [1, 5 ..]

{- | <http://oeis.org/A017817>

a(n) = a(n-3) + a(n-4), with a(0)=1, a(1)=a(2)=0, a(3)=1

> [1,0,0,1,1,0,1,2,1,1,3,3,2,4,6,5,6,10,11,11,16,21,22,27,37,43,49,64,80,92] `isPrefixOf` a017817
-}
a017817 :: Num n => [n]
a017817 = 1 : 0 : 0 : 1 : zipWith (+) a017817 (tail a017817)

{- | <http://oeis.org/A020695>

Pisot sequence E(2,3).

> [2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711] `isPrefixOf` a020695
-}
a020695 :: Num n => [n]
a020695 = drop 3 a000045

{- | <https://oeis.org/A020985>

The Rudin-Shapiro or Golay-Rudin-Shapiro sequence (coefficients of the Shapiro polynomials).		45

> [1, 1, 1, -1, 1, 1, -1, 1, 1, 1, 1, -1, -1, -1, 1, -1, 1, 1, 1, -1, 1, 1, -1, 1, -1, -1, -1, 1, 1, 1, -1, 1, 1, 1, 1, -1] `isPrefixOf` a020985
-}
a020985 :: [Integer]
a020985 =
  let f (x:xs) w = x : x*w : f xs (0 - w)
      f [] _ = error "a020985?"
  in 1 : 1 : f (tail a020985) (-1)

{- | <http://oeis.org/A022095>

Fibonacci sequence beginning 1, 5.

> [1,5,6,11,17,28,45,73,118,191,309,500,809,1309,2118,3427,5545,8972,14517,23489] `isPrefixOf` a022095
-}
a022095 :: Num n => [n]
a022095 = 1 : 5 : zipWith (+) a022095 (tail a022095)

{- | <http://oeis.org/A022096>

Fibonacci sequence beginning 1, 6.

> [1,6,7,13,20,33,53,86,139,225,364,589,953,1542,2495,4037,6532,10569,17101,27670] `isPrefixOf` a022096
-}
a022096 :: Num n => [n]
a022096 = 1 : 6 : zipWith (+) a022096 (tail a022096)

{- | <https://oeis.org/A027750>

Triangle read by rows in which row n lists the divisors of n.

> [1,1,2,1,3,1,2,4,1,5,1,2,3,6,1,7,1,2,4,8,1,3,9,1,2,5,10,1,11,1,2,3,4,6,12,1,13] `isPrefixOf` a027750
-}
a027750 :: Integral n => [n]
a027750 = concatMap a027750_row [1..]

a027750_row :: Integral n => n -> [n]
a027750_row n = filter ((== 0) . (mod n)) [1..n]

{- | <http://oeis.org/A027934>

a(0)=0, a(1)=1, a(2)=2; for n > 2, a(n) = 3*a(n-1) - a(n-2) - 2*a(n-3).

> [0,1,2,5,11,24,51,107,222,457,935,1904,3863,7815,15774,31781,63939,128488] `isPrefixOf` a027934
-}
a027934 :: Num n => [n]
a027934 =
  let f x y z = 3 * x - y - 2 * z
  in 0 : 1 : 2 : zipWith3 f (drop 2 a027934) (tail a027934) a027934

{- | <http://oeis.org/A029635>

The (1,2)-Pascal triangle (or Lucas triangle) read by rows.

> [2,1,2,1,3,2,1,4,5,2,1,5,9,7,2,1,6,14,16,9,2,1,7,20,30,25,11,2,1,8,27,50,55,36] `isPrefixOf` a029635
> take 7 a029635_tbl == [[2],[1,2],[1,3,2],[1,4,5,2],[1,5,9,7,2],[1,6,14,16,9,2],[1,7,20,30,25,11,2]]
-}
a029635 :: Num i => [i]
a029635 = concat a029635_tbl

a029635_tbl :: Num i => [[i]]
a029635_tbl =
  let f r = zipWith (+) (0 : r) (r ++ [0])
  in [2] : iterate f [1,2]

{- | <http://oeis.org/A030308>

Triangle T(n,k): Write n in base 2, reverse order of digits, to get the n-th row

> take 9 a030308 == [[0],[1],[0,1],[1,1],[0,0,1],[1,0,1],[0,1,1],[1,1,1],[0,0,0,1]]
-}
a030308 :: (Eq n,Num n) => [[n]]
a030308 =
   let f l = case l of
         [] -> [1]
         0:b -> 1 : b
         1:b -> 0 : f b
         _ -> error "A030308"
   in iterate f [0]

{- | <https://oeis.org/A033622>

Good sequence of increments for Shell sort (best on big values).

[1, 5, 19, 41, 109, 209, 505, 929, 2161, 3905, 8929, 16001, 36289, 64769, 146305, 260609, 587521] `isPrefixOf` a033622
-}
a033622 :: [Integer]
a033622 = map a033622_n [0..]

a033622_n :: Integer -> Integer
a033622_n n =
  if even n
  then 9 * 2 ^ n - 9 * 2 ^ ( n `div` 2) + 1
  else 8 * 2 ^ n - 6 * 2 ^ ((n + 1 )`div` 2) + 1

{- | <http://oeis.org/A033812>

The Loh-Shu 3 X 3 magic square, lexicographically largest variant when read by columns.
-}
a033812 :: Num n => [n]
a033812 = [8, 1, 6, 3, 5, 7, 4, 9, 2]

{- | <http://oeis.org/A034968>

Minimal number of factorials that add to n.

> [0,1,1,2,2,3,1,2,2,3,3,4,2,3,3,4,4,5,3,4,4,5,5,6,1,2,2,3,3,4,2,3,3,4,4,5,3,4,4] `isPrefixOf` a034968
-}
a034968 :: Integral n => [n]
a034968 =
  let f i s n = if n == 0 then s else f (i + 1) (s + rem n i) (quot n i)
  in map (f 2 0) [0 ..]

{- | <https://oeis.org/A036562>

a(n) = 4^(n+1) + 3*2^n + 1

[1, 8, 23, 77, 281, 1073, 4193, 16577, 65921, 262913, 1050113, 4197377, 16783361, 67121153] `isPrefixOf` a036562
-}
a036562 :: [Integer]
a036562 = 1 : map a036562_n [0..]

a036562_n :: Integer -> Integer
a036562_n n = 4^(n+1) + 3*2^n + 1

{- | <http://oeis.org/A046042>

Number of partitions of n into fourth powers.

> [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3] `isPrefixOf` a046042
-}
a046042 :: Num n => [n]
a046042 =
  let p _ 0 = 1
      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m
      p _ _ = error "A046042"
  in map (p (tail a000583)) [1::Integer ..]

{- | <http://oeis.org/A047999>

Sierpiński's triangle (or gasket): triangle, read by rows, formed by reading Pascal's triangle mod 2.

> [1,1,1,1,0,1,1,1,1,1,1,0,0,0,1,1,1,0,0,1,1,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0] `isPrefixOf` a047999
-}
a047999 :: [Int]
a047999 = concat a047999_tbl

a047999_tbl :: [[Int]]
a047999_tbl = iterate (\r -> zipWith xor (0 : r) (r ++ [0])) [1]

{- | <https://oeis.org/A048993>

Triangle of Stirling numbers of 2nd kind, S(n,k), n >= 0, 0 <= k <= n.

> [1,0,1,0,1,1,0,1,3,1,0,1,7,6,1,0,1,15,25,10,1,0,1,31,90,65,15,1] `isPrefixOf` a048993
-}
a048993 :: (Enum n,Num n) => [n]
a048993 = concat a048993_tbl

a048993_tbl :: (Enum n,Num n) => [[n]]
a048993_tbl = iterate (\row -> 0 : zipWith (+) row (zipWith (*) [1..] (tail row)) ++ [1]) [1]

{- | <http://oeis.org/A049455>

Triangle read by rows, numerator of fractions of a variant of the Farey series.

> [0,1,0,1,1,0,1,1,2,1,0,1,1,2,1,3,2,3,1,0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,0] `isPrefixOf` a049455
> plot_p1_imp [take 200 (a049455 :: [Int])]
> plot_p1_pt [take 10000 (a049455 :: [Int])]
-}
a049455 :: Integral n => [n]
a049455 = map fst (concat Math.stern_brocot_tree_lhs)

{- | <http://oeis.org/A049456>

Triangle read by rows, denominator of fractions of a variant of the Farey series.

[1,1,1,2,1,1,3,2,3,1,1,4,3,5,2,5,3,4,1,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,1,6,5,9] `isPrefixOf` a049456
> plot_p1_imp [take 200 (a049456 :: [Int])]
> plot_p1_pt [take 10000 (a049456 :: [Int])]
-}
a049456 :: Integral n => [n]
a049456 = map snd (concat Math.stern_brocot_tree_lhs)

{- | <http://oeis.org/A053121>

Catalan triangle (with 0's) read by rows.

> [1,0,1,1,0,1,0,2,0,1,2,0,3,0,1,0,5,0,4,0,1,5,0,9,0,5,0,1,0,14,0,14,0,6,0,1,14,0] `isPrefixOf` a053121
> take 7 a053121_tbl == [[1],[0,1],[1,0,1],[0,2,0,1],[2,0,3,0,1],[0,5,0,4,0,1],[5,0,9,0,5,0,1]]
-}
a053121 :: Num n => [n]
a053121 = concat a053121_tbl

a053121_tbl :: Num n => [[n]]
a053121_tbl = iterate (\row -> zipWith (+) (0 : row) (tail row ++ [0, 0])) [1]

{- | <http://oeis.org/A058265>

Decimal expansion of the tribonacci constant t, the real root of x^3 - x^2 - x - 1.

> [1,8,3,9,2,8,6,7,5,5,2,1,4,1,6,1,1,3,2,5,5,1,8,5,2,5,6,4,6,5,3,2,8,6,6,0,0,4,2] `isPrefixOf` a058265

> a058265_k :: Data.Number.Fixed.Fixed Data.Number.Fixed.Prec500 {- numbers -}
-}
a058265 :: Num n => [n]
a058265 = map (fromIntegral . digitToInt) "183928675521416113255185256465328660042417874609759224677875863940420322208196642573843541942830701414197982685924097416417845074650743694383154582049951379624965553964461366612154027797267811894104121160922328215595607181671218236598665227337853781569698925211739579141322872106187898408525495693114534913498534595761750359652213238142472727224173581877000697905510254904496571074252654772281100659893755563630933305282623575385197199429914530082546639774729005870059744813919316728258488396263329709" ++ error "A058265"

-- | A058265 as 'Floating' calculation, see "Data.Number.Fixed".
a058265_k :: Floating n => n
a058265_k = (1/3) * (1 + (19 + 3 * sqrt 33) ** (1/3) + (19 - 3 * sqrt 33)  ** (1/3))

{- | <http://oeis.org/A060588>

If the final two digits of n written in base 3 are the same then this digit, otherwise mod 3-sum of these two digits.

> [0,2,1,2,1,0,1,0,2,0,2,1,2,1,0,1,0,2,0,2,1,2,1,0,1,0,2,0,2,1,2,1,0,1,0,2,0,2,1] `isPrefixOf` a060588a
-}
a060588a :: Integral n => [n]
a060588a = map a060588a_n [0..]

a060588a_n :: Integral n => n -> n
a060588a_n n = (-n - floor (fromIntegral n / (3::Double))) `mod` 3

{- | <http://oeis.org/A061654>

a(n) = (3*16^n + 2)/5

> [1,10,154,2458,39322,629146,10066330,161061274,2576980378,41231686042] `isPrefixOf` a061654
-}
a061654 :: Integral n => [n]
a061654 = map a061654_n [0 ..]

a061654_n :: Integral n => n -> n
a061654_n n = (3 * 16^n + 2) `div` 5

{- | <http://oeis.org/A064413>

EKG sequence (or ECG sequence): a(1) = 1; a(2) = 2; for n > 2, a(n) = smallest number not already used which shares a factor with a(n-1).

> [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11,33,27,30,25,35,28,26,13,39,36,32,34,17] `isPrefixOf` a064413
> plot_p1_ln [take 200 a064413 :: [Int]]
> plot_p1_pt [take 2000 a064413 :: [Int]]
-}
a064413 :: Integral n => [n]
a064413 =
  let ekg x zs =
        let f (y:ys) =
              if gcd x y > 1
              then y : ekg y (delete y zs)
              else f ys
            f [] = error "?"
        in f zs
  in 1 : ekg 2 [2..]

{- | <http://oeis.org/A071996>

a(1) = 0, a(2) = 1, a(n) = a(floor(n/3)) + a(n - floor(n/3)).

> [0,1,1,1,1,2,2,3,3,3,4,4,4,4,4,5,5,6,6,6,6,6,7,8,8,9,9,9,9,9,9,9,10,11,12,12,12] `isPrefixOf` a071996

> plot_p1_ln [take 50 a000201 :: [Int]]
> plot_p1_imp [map length (take 250 (group a071996))]
-}
a071996 :: Integral n => [n]
a071996 =
  let f n =
        case n of
          0 -> error "A071996"
          1 -> 0
          2 -> 1
          _ -> let m = floor (fromIntegral n / (3::Double)) in f m + f (n - m)
  in map f [1::Int ..]

{- | <http://oeis.org/A073334>

The "rhythmic infinity system" of Danish composer Per Nørgård

> take 24 a073334 == [3,5,8,5,8,13,8,5,8,13,21,13,8,13,8,5,8,13,21,13,21,34,21,13]
> plot_p1_imp [take 200 (a073334 :: [Int])]
-}
a073334 :: Num n => [n]
a073334 =
  let f n = a000045 !! ((a005811 !! n) + 4)
  in 3 : map f [1..]

{- | <https://oeis.org/A080843>

Tribonacci word: limit S(infinity), where S(0) = 0, S(1) = 0,1, S(2) = 0,1,0,2 and for n >= 0, S(n+3) = S(n+2) S(n+1) S(n).

> [0,1,0,2,0,1,0,0,1,0,2,0,1,0,1,0,2,0,1,0,0,1,0,2,0,1,0,2,0,1,0,0,1,0,2,0,1,0,1] `isPrefixOf` a080843
-}
a080843 :: Integral n => [n]
a080843 =
  let rw n = case n of {0 -> [0,1];1 -> [0,2];2 -> [0];_ -> error "A080843"}
      unf = let f n l = case l of {[] -> error "A080843";x:xs -> drop n x ++ f (length x) xs} in f 0
  in unf (iterate (concatMap rw) [0])

{- | <http://oeis.org/A080992>

Entries in Durer's magic square.

> [16,3,2,13,5,10,11,8,9,6,7,12,4,15,14,1] == a080992
-}
a080992 :: Num n => [n]
a080992 =
  [16,03,02,13
  ,05,10,11,08
  ,09,06,07,12
  ,04,15,14,01]

{- | <http://oeis.org/A083866>

Positions of zeros in Per Nørgård's infinity sequence (A004718).

> take 24 a083866 == [0,5,10,17,20,27,34,40,45,54,65,68,75,80,85,90,99,105,108,119,130,136,141,150]
-}
a083866 :: (Enum n,Num n) => [n]
a083866 = map snd (filter ((== (0::Int)) . fst) (zip a004718 [0..]))

{- | <http://oeis.org/A095660>

Pascal (1,3) triangle.

> [3,1,3,1,4,3,1,5,7,3,1,6,12,10,3,1,7,18,22,13,3,1,8,25,40,35,16,3,1,9,33,65,75] `isPrefixOf` a095660
> take 6 a095660_tbl == [[3],[1,3],[1,4,3],[1,5,7,3],[1,6,12,10,3],[1,7,18,22,13,3]]
-}
a095660 :: Num i => [i]
a095660 = concat a095660_tbl

a095660_tbl :: Num i => [[i]]
a095660_tbl =
  let f r = zipWith (+) (0 : r) (r ++ [0])
  in [3] : iterate f [1,3]

{- | <http://oeis.org/A095666>

Pascal (1,4) triangle.

> [4,1,4,1,5,4,1,6,9,4,1,7,15,13,4,1,8,22,28,17,4,1,9,30,50,45,21,4,1,10,39,80,95] `isPrefixOf` a095666
> take 6 a095666_tbl == [[4],[1,4],[1,5,4],[1,6,9,4],[1,7,15,13,4],[1,8,22,28,17,4]]
-}
a095666 :: Num i => [i]
a095666 = concat a095666_tbl

a095666_tbl :: Num i => [[i]]
a095666_tbl =
  let f r = zipWith (+) (0 : r) (r ++ [0])
  in [4] : iterate f [1,4]

{- | <http://oeis.org/A096940>

Pascal (1,5) triangle.

> [5,1,5,1,6,5,1,7,11,5,1,8,18,16,5,1,9,26,34,21,5,1,10,35,60,55,26,5,1,11,45,95] `isPrefixOf` a096940
> take 6 a096940_tbl == [[5],[1,5],[1,6,5],[1,7,11,5],[1,8,18,16,5],[1,9,26,34,21,5]]
-}
a096940 :: Num i => [i]
a096940 = concat a096940_tbl

a096940_tbl :: Num i => [[i]]
a096940_tbl =
  let f r = zipWith (+) (0 : r) (r ++ [0])
  in [5] : iterate f [1,5]

{- | http://oeis.org/A098550

The Yellowstone permutation: a(n) = n if n <= 3, otherwise the smallest number not occurring earlier having at least one common factor with a(n-2), but none with a(n-1).

> [1,2,3,4,9,8,15,14,5,6,25,12,35,16,7,10,21,20,27,22,39,11,13,33,26,45,28,51,32,17,18,85,24,55] `isPrefixOf` a098550
> plot_p1_ln [take 200 (a098550 :: [Int])]
> plot_p1_pt [take 5000 (a098550 :: [Int])]
-}
a098550 :: Integral n => [n]
a098550 =
  let f u v ws =
        let g [] = error "?"
            g (x:xs) =
              if gcd x u > 1 && gcd x v == 1
              then x : f v x (delete x ws)
              else g xs
        in g ws
  in 1 : 2 : 3 : f 2 3 [4 ..]

{- | <http://oeis.org/A105809>

A Fibonacci-Pascal matrix.

> [1,1,1,2,2,1,3,4,3,1,5,7,7,4,1,8,12,14,11,5,1,13,20,26,25,16,6,1,21,33,46,51,41] `isPrefixOf` a105809
-}
a105809 :: Num n => [n]
a105809 = concat a105809_tbl

a105809_tbl :: Num n => [[n]]
a105809_tbl =
  let f (u:_, vs) = (vs, zipWith (+) (u : vs) (vs ++ [0]))
      f _ = error "A105809"
  in map fst (iterate f ([1], [1, 1]))

{- | <http://oeis.org/A124010>

Triangle in which first row is 0, n-th row (n>1) lists the (ordered)
prime signature of n, that is, the exponents of distinct prime factors
in factorization of n.

> [0,1,1,2,1,1,1,1,3,2,1,1,1,2,1,1,1,1,1,1,4,1,1,2,1,2,1,1,1,1,1,1,3,1,2,1,1,3,2,1,1,1,1,1,1,5,1] `isPrefixOf` a124010
-}
a124010 :: Integral n => [n]
a124010 = concatMap a124010_row [1..]

a124010_row :: Integral n => n -> [n]
a124010_row n =
  let f u w =
        case (u, w) of
          (1, _) -> []
          (_, p:ps) ->
            let h v e =
                  let (v', m) = divMod v p
                  in if m == 0
                     then h v' (e + 1)
                     else if e > 0
                          then e : f v ps
                          else f v ps
            in h u 0
          _ -> error "a124010"
  in if n == 1 then [0] else f n a000040

{- | <https://oeis.org/A124472>

Benjamin Franklin's 16 X 16 magic square read by rows.

> [200,217,232,249,8,25,40,57,72,89,104,121,136,153,168,185,58,39,26,7,250,231] `isPrefixOf` a124472
-}
a124472 :: Num n => [n]
a124472 =
  concat
  [[200,217,232,249,8,25,40,57,72,89,104,121,136,153,168,185]
  ,[58,39,26,7,250,231,218,199,186,167,154,135,122,103,90,71]
  ,[198,219,230,251,6,27,38,59,70,91,102,123,134,155,166,187]
  ,[60,37,28,5,252,229,220,197,188,165,156,133,124,101,92,69]
  ,[201,216,233,248,9,24,41,56,73,88,105,120,137,152,169,184]
  ,[55,42,23,10,247,234,215,202,183,170,151,138,119,106,87,74]
  ,[203,214,235,246,11,22,43,54,75,86,107,118,139,150,171,182]
  ,[53,44,21,12,245,236,213,204,181,172,149,140,117,108,85,76]
  ,[205,212,237,244,13,20,45,52,77,84,109,116,141,148,173,180]
  ,[51,46,19,14,243,238,211,206,179,174,147,142,115,110,83,78]
  ,[207,210,239,242,15,18,47,50,79,82,111,114,143,146,175,178]
  ,[49,48,17,16,241,240,209,208,177,176,145,144,113,112,81,80]
  ,[196,221,228,253,4,29,36,61,68,93,100,125,132,157,164,189]
  ,[62,35,30,3,254,227,222,195,190,163,158,131,126,99,94,67]
  ,[194,223,226,255,2,31,34,63,66,95,98,127,130,159,162,191]
  ,[64,33,32,1,256,225,224,193,192,161,160,129,128,97,96,65]]

{- | <http://oeis.org/A125519>

A 4 x 4 permutation-free magic square.
-}
a125519 :: Num n => [n]
a125519 = [831,326,267,574,584,257,316,841,158,683,742,415,425,732,673,168]

{- | <http://oeis.org/A126275>

Moment of inertia of all magic squares of order n.

> [5,60,340,1300,3885,9800,21840,44280,83325,147620,248820,402220,627445,949200] `isPrefixOf` a126275
-}
a126275 :: Integral n => [n]
a126275 = map a126275_n [2..]

a126275_n :: Integral n => n -> n
a126275_n n = (n ^ (2::Int) * (n ^ (4::Int) - 1)) `div` 12

{- | <http://oeis.org/A126276>

Moment of inertia of all magic cubes of order n.

> [18,504,5200,31500,136710,471968,1378944,3547800,8258250,17728920,35603568] `isPrefixOf` a126276
-}
a126276 :: Integral n => [n]
a126276 = map a126276_n [2..]

a126276_n :: Integral n => n -> n
a126276_n n = (n ^ (3::Int) * (n ^ (3::Int) + 1) * (n ^ (2::Int) - 1)) `div` 12

{- | <http://oeis.org/A126651>

A 7 x 7 magic square.
-}
a126651 :: Num n => [n]
a126651 =
  [71,  1, 51, 32, 50,  2, 80
  ,21, 41, 61, 56, 26, 13, 69
  ,31, 81, 11, 20, 62, 65, 17
  ,34, 40, 60, 43, 28, 64, 18
  ,48, 42, 22, 54, 39, 75,  7
  ,33, 53, 15, 68, 16, 44, 58
  ,49, 29, 67, 14, 66, 24, 38]

{- | <http://oeis.org/A126652>

A 3 X 3 magic square with magic sum 75: the Loh-Shu square A033812 multiplied by 5.

> a126652 == map (* 5) a033812
-}
a126652 :: Num n => [n]
a126652 = [40, 5, 30, 15, 25, 35, 20, 45, 10]

{- | <http://oeis.org/A126653>

A 3 X 3 magic square with magic sum 45: the Loh-Shu square A033812 multiplied by 3.

> a126653 == map (* 3) a033812
-}
a126653 :: Num n => [n]
a126653 = [24, 3, 18, 9, 15, 21, 12, 27, 6]

{- | <http://oeis.org/A126654>

A 3 x 3 magic square.
-}
a126654 :: Num n => [n]
a126654 = [32, 4, 24, 12, 20, 28, 16, 36, 8]

{- | <http://oeis.org/A126709>

The Loh-Shu 3 x 3 magic square, variant 2.

Loh-Shu magic square, attributed to the legendary Fu Xi (Fuh-Hi).
-}
a126709 :: Num n => [n]
a126709 =
  [4,9,2
  ,3,5,7
  ,8,1,6]

{- | <http://oeis.org/A126710>

Jaina inscription of the twelfth or thirteenth century, Khajuraho, India.
-}
a126710 :: Num n => [n]
a126710 =
  [ 7,12, 1,14
  , 2,13, 8,11
  ,16, 3,10, 5
  , 9, 6,15, 4]

{- | <http://oeis.org/A126976>

A 6 x 6 magic square read by rows.

Agrippa (Magic Square of the Sun)
-}
a126976 :: Num n => [n]
a126976 =
  [06,32,03,34,35,01
  ,07,11,27,28,08,30
  ,19,14,16,15,23,24
  ,18,20,22,21,17,13
  ,25,29,10,09,26,12
  ,36,05,33,04,02,31]

{- | <https://oeis.org/A212804>

Expansion of (1 - x)/(1 - x - x^2).

[1,0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946] `isPrefixOf` a212804
-}
a212804 :: Integral n => [n]
a212804 = 1 : a000045

{- | <https://oeis.org/A245553>

A Rauzy fractal sequence: trajectory of 1 under morphism 1 -> 2,3; 2 -> 3; 3 -> 1.

> [1,2,3,2,3,3,1,2,3,3,1,3,1,1,2,3,2,3,3,1,3,1,1,2,3,3,1,1,2,3,1,2,3,2,3,3,1,2,3] `isPrefixOf` a245553
-}
a245553 :: Integral n => [n]
a245553 =
  let rw n = case n of {1 -> [2,3];2 -> [3];3 -> [1];_ -> error "A245553"}
      jn x = x ++ concatMap rw x
      unf = let f n l = case l of {[] -> error "A245553";x:xs -> drop n x ++ f (length x) xs} in f 0
  in unf (iterate jn [1])

{- | <http://oeis.org/A255723>

Another variant of Per Nørgård's "infinity sequence"

> take 24 a255723 == [0,-2,-1,2,-2,-4,1,0,-1,-3,0,1,2,0,-3,4,-2,-4,1,0,-4,-6,3,-2]
> plot_p1_imp [take 400 (a255723 :: [Int])]
-}
a255723 :: Num n => [n]
a255723 = 0 : concat (transpose [map (subtract 2) a255723
                                ,map (-1 -) a255723
                                ,map (+ 2) a255723
                                ,tail a255723])

{- | <http://oeis.org/A256184>

First of two variations by Per Nørgård of his "infinity sequence"

> take 24 a256184 == [0,-2,-1,2,-4,-3,1,-3,-2,-2,0,1,4,-6,-5,3,-5,-4,-1,-1,0,3,-5,-4]
-}
a256184 :: Num n => [n]
a256184 = 0 : concat (transpose [map (subtract 2) a256184
                                ,map (subtract 1) a256184
                                ,map negate (tail a256184)])

{- | <http://oeis.org/A256185>

Second of two variations by Per Nørgård of his "infinity sequence"

> take 24 a256185 == [0,-3,-2,3,-6,1,2,-5,0,-3,0,-5,6,-9,4,-1,-2,-3,-2,-1,-4,5,-8,3]
-}
a256185 :: Num n => [n]
a256185 = 0 : concat (transpose [map (subtract 3) a256185
                                ,map (-2 -) a256185
                                ,map negate (tail a256185)])

{- | <http://oeis.org/A270876>

Number of magic tori of order n composed of the numbers from 1 to n^2.

> [1,0,1,255,251449712] == a270876
-}
a270876 :: Integral n => [n]
a270876 = [1,0,1,255,251449712]

{- | <http://oeis.org/A320872>

For all possible 3 X 3 magic squares made of primes, in order of increasing magic sum, list the lexicographically smallest representative of each equivalence class (modulo symmetries of the square), as a row of the 9 elements (3 rows of 3 elements each).
-}
a320872 :: Num n => [n]
a320872 =
  [17, 89,  71,  113,  59,  5, 47, 29, 101
  ,41, 89,  83,  113,  71, 29, 59, 53, 101
  ,37, 79,  103, 139,  73,  7, 43, 67, 109
  ,29, 131, 107, 167,  89, 11, 71, 47, 149
  ,43, 127, 139, 199, 103,  7, 67, 79, 163
  ,37, 151, 139, 211, 109,  7, 79, 67, 181
  ,43, 181, 157, 241, 127, 13, 97, 73, 211]
