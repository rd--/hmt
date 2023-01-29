-- | Godfried T. Toussaint et. al.
-- \"The distance geometry of music\"
-- /Journal of Computational Geometry: Theory and Applications/
-- Volume 42, Issue 5, July, 2009
-- (<http://dx.doi.org/10.1016/j.comgeo.2008.04.005>)
module Music.Theory.Bjorklund where

import Data.List.Split {- split -}

import qualified Music.Theory.List as T {- hmt-base -}

-- | Bjorklund state
type Bjorklund a = ((Int, Int), ([[a]], [[a]]))

-- | Bjorklund left process
bjorklund_left_f :: Bjorklund a -> Bjorklund a
bjorklund_left_f ((i,j),(xs,ys)) =
    let (xs',xs'') = splitAt j xs
    in ((j,i-j),(zipWith (++) xs' ys,xs''))

-- | Bjorklund right process
bjorklund_right_f :: Bjorklund a -> Bjorklund a
bjorklund_right_f ((i,j),(xs,ys)) =
    let (ys',ys'') = splitAt i ys
    in ((i,j-i),(zipWith (++) xs ys',ys''))

-- | Bjorklund process, left & recur or right & recur or halt.
bjorklund_f :: Bjorklund a -> Bjorklund a
bjorklund_f (n,x) =
    let (i,j) = n
    in if min i j <= 1
       then (n,x)
       else bjorklund_f (if i > j then bjorklund_left_f (n,x) else bjorklund_right_f (n,x))

{- | Bjorklund's algorithm to construct a binary sequence of /n/ bits
with /k/ ones such that the /k/ ones are distributed as evenly as
possible among the (/n/ - /k/) zeroes.

> bjorklund (5, 9) == [True,False,True,False,True,False,True,False,True]
> map xdot_ascii (bjorklund (5, 9)) == "x.x.x.x.x"

> let es = [(2,[3,5]),(3,[4,5,8]),(4,[7,9,12,15]),(5,[6,7,8,9,11,12,13,16]),(6,[7,13]),(7,[8,9,10,12,15,16,17,18]),(8,[17,19]),(9,[14,16,22,23]),(11,[12,24]),(13,[24]),(15,[34])]
> let es' = concatMap (\(i,j) -> map ((,) i) j) es
> mapM_ (putStrLn . euler_pp_unicode) es'

> > E(2,3) [××·] (12)
> > E(2,5) [×·×··] (23)
> > E(3,4) [×××·] (112)
> > E(3,5) [×·×·×] (221)
> > E(3,8) [×··×··×·] (332)
> > E(4,7) [×·×·×·×] (2221)
> > E(4,9) [×·×·×·×··] (2223)
> > E(4,12) [×··×··×··×··] (3333)
> > E(4,15) [×···×···×···×··] (4443)
> > E(5,6) [×××××·] (11112)
> > E(5,7) [×·××·××] (21211)
> > E(5,8) [×·××·××·] (21212)
> > E(5,9) [×·×·×·×·×] (22221)
> > E(5,11) [×·×·×·×·×··] (22223)
> > E(5,12) [×··×·×··×·×·] (32322)
> > E(5,13) [×··×·×··×·×··] (32323)
> > E(5,16) [×··×··×··×··×···] (33334)
> > E(6,7) [××××××·] (111112)
> > E(6,13) [×·×·×·×·×·×··] (222223)
> > E(7,8) [×××××××·] (1111112)
> > E(7,9) [×·×××·×××] (2112111)
> > E(7,10) [×·××·××·××] (2121211)
> > E(7,12) [×·××·×·××·×·] (2122122)
> > E(7,15) [×·×·×·×·×·×·×··] (2222223)
> > E(7,16) [×··×·×·×··×·×·×·] (3223222)
> > E(7,17) [×··×·×··×·×··×·×·] (3232322)
> > E(7,18) [×··×·×··×·×··×·×··] (3232323)
> > E(8,17) [×·×·×·×·×·×·×·×··] (22222223)
> > E(8,19) [×··×·×·×··×·×·×··×·] (32232232)
> > E(9,14) [×·××·××·××·××·] (212121212)
> > E(9,16) [×·××·×·×·××·×·×·] (212221222)
> > E(9,22) [×··×·×··×·×··×·×··×·×·] (323232322)
> > E(9,23) [×··×·×··×·×··×·×··×·×··] (323232323)
> > E(11,12) [×××××××××××·] (11111111112)
> > E(11,24) [×··×·×·×·×·×··×·×·×·×·×·] (32222322222)
> > E(13,24) [×·××·×·×·×·×·××·×·×·×·×·] (2122222122222)
> > E(15,34) [×··×·×·×·×··×·×·×·×··×·×·×·×··×·×·] (322232223222322)

-}
bjorklund :: (Int, Int) -> [Bool]
bjorklund (i,j') =
    let j = j' - i
        x = replicate i [True]
        y = replicate j [False]
        (_,(x',y')) = bjorklund_f ((i,j),(x,y))
    in concat x' ++ concat y'

{- | 'T.rotate_right' of 'bjorklund'.

> map xdot_unicode (bjorklund_r 2 (5,16)) == "··×··×··×··×··×·"
-}
bjorklund_r :: Int -> (Int, Int) -> [Bool]
bjorklund_r n = T.rotate_right n . bjorklund

-- | Pretty printer, generalise.
euler_pp_f :: (Bool -> Char) -> (Int,Int) -> String
euler_pp_f f e =
    let r = bjorklund e
    in concat ["E",show e," [",map f r,"] ",iseq_str r]

{- | Unicode form, ie. @×·@.

> euler_pp_unicode (7,12) == "E(7,12) [×·××·×·××·×·] (2122122)"
-}
euler_pp_unicode :: (Int, Int) -> String
euler_pp_unicode = euler_pp_f xdot_unicode

{- | ASCII form, ie. @x.@.

> euler_pp_ascii (7,12) == "E(7,12) [x.xx.x.xx.x.] (2122122)"
-}
euler_pp_ascii :: (Int, Int) -> String
euler_pp_ascii = euler_pp_f xdot_ascii

{- | /xdot/ notation for pattern.

> map xdot_ascii (bjorklund (5,9)) == "x.x.x.x.x"
-}
xdot_ascii :: Bool -> Char
xdot_ascii x = if x then 'x' else '.'

{- | Unicode variant.

> map xdot_unicode (bjorklund (5,12)) == "×··×·×··×·×·"
> map xdot_unicode (bjorklund (5,16)) == "×··×··×··×··×···"
-}
xdot_unicode :: Bool -> Char
xdot_unicode x = if x then '×' else '·'

{- | The 'iseq' of a pattern is the distance between 'True' values.

> iseq (bjorklund (5,9)) == [2,2,2,2,1]
-}
iseq :: [Bool] -> [Int]
iseq = let f = split . keepDelimsL . whenElt in tail . map length . f (== True)

{- | 'iseq' of pattern as compact string.

> iseq_str (bjorklund (5,9)) == "(22221)"
-}
iseq_str :: [Bool] -> String
iseq_str = let f xs = "(" ++ concatMap show xs ++ ")" in f . iseq

{- | Bresenham's algorithm.

> bresenham (5, 12) == bjorklund(5, 12)

Given es' above compare algorithms:

> map (\p -> bresenham p == bjorklund p) es'
> let vw = map xdot_ascii
> mapM_ (\p -> let (a, b) = (bresenham p, bjorklund p) in print (p, vw a, vw b, a == b)) es'

> > ((2,3),"x.x","xx.",False)
> > ((2,5),"x..x.","x.x..",False)
> > ((3,4),"x.xx","xxx.",False)
> > ((3,5),"x.x.x","x.x.x",True)
> > ((3,8),"x..x..x.","x..x..x.",True)
> > ((4,7),"x.x.x.x","x.x.x.x",True)
> > ((4,9),"x..x.x.x.","x.x.x.x..",False)
> > ((4,12),"x..x..x..x..","x..x..x..x..",True)
> > ((4,15),"x...x...x...x..","x...x...x...x..",True)
> > ((5,6),"x.xxxx","xxxxx.",False)
> > ((5,7),"x.xx.xx","x.xx.xx",True)
> > ((5,8),"x.x.xx.x","x.xx.xx.",False)
> > ((5,9),"x.x.x.x.x","x.x.x.x.x",True)
> > ((5,11),"x..x.x.x.x.","x.x.x.x.x..",False)
> > ((5,12),"x..x.x..x.x.","x..x.x..x.x.",True)
> > ((5,13),"x..x..x.x..x.","x..x.x..x.x..",False)
> > ((5,16),"x...x..x..x..x..","x..x..x..x..x...",False)
> > ((6,7),"x.xxxxx","xxxxxx.",False)
> > ((6,13),"x..x.x.x.x.x.","x.x.x.x.x.x..",False)
> > ((7,8),"x.xxxxxx","xxxxxxx.",False)
> > ((7,9),"x.xxx.xxx","x.xxx.xxx",True)
> > ((7,10),"x.xx.xx.xx","x.xx.xx.xx",True)
> > ((7,12),"x.x.x.xx.x.x","x.xx.x.xx.x.",False)
> > ((7,15),"x..x.x.x.x.x.x.","x.x.x.x.x.x.x..",False)
> > ((7,16),"x..x.x.x..x.x.x.","x..x.x.x..x.x.x.",True)
> > ((7,17),"x..x.x..x.x..x.x.","x..x.x..x.x..x.x.",True)
> > ((7,18),"x..x..x.x..x.x..x.","x..x.x..x.x..x.x..",False)
> > ((8,17),"x..x.x.x.x.x.x.x.","x.x.x.x.x.x.x.x..",False)
> > ((8,19),"x..x.x..x.x.x..x.x.","x..x.x.x..x.x.x..x.",False)
> > ((9,14),"x.x.xx.xx.xx.x","x.xx.xx.xx.xx.",False)
> > ((9,16),"x.x.x.x.xx.x.x.x","x.xx.x.x.xx.x.x.",False)
> > ((9,22),"x..x.x..x.x..x.x..x.x.","x..x.x..x.x..x.x..x.x.",True)
> > ((9,23),"x..x..x.x..x.x..x.x..x.","x..x.x..x.x..x.x..x.x..",False)
> > ((11,12),"x.xxxxxxxxxx","xxxxxxxxxxx.",False)
> > ((11,24),"x..x.x.x.x.x..x.x.x.x.x.","x..x.x.x.x.x..x.x.x.x.x.",True)
> > ((13,24),"x.x.x.x.x.x.xx.x.x.x.x.x","x.xx.x.x.x.x.xx.x.x.x.x.",False)
> > ((15,34),"x..x.x.x..x.x.x.x..x.x.x.x..x.x.x.","x..x.x.x.x..x.x.x.x..x.x.x.x..x.x.",False)
-}
bresenham :: (Int, Int) -> [Bool]
bresenham (s, n) =
  let f d = if d >= 0 then True : f (d + s - n) else False : f (d + s)
  in take n (f 0)
