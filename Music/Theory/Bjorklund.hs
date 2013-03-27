-- | Godfried T. Toussaint et. al.
-- \"The distance geometry of music\"
-- /Journal of Computational Geometry: Theory and Applications/
-- Volume 42, Issue 5, July, 2009
-- (<http://dx.doi.org/10.1016/j.comgeo.2008.04.005>)
module Music.Theory.Bjorklund (bjorklund,xdot,iseq,iseq_str) where

import Data.List.Split {- split -}

type STEP a = ((Int,Int),([[a]],[[a]]))

left :: STEP a -> STEP a
left ((i,j),(xs,ys)) =
    let (xs',xs'') = splitAt j xs
    in ((j,i-j),(zipWith (++) xs' ys,xs''))

right :: STEP a -> STEP a
right ((i,j),(xs,ys)) =
    let (ys',ys'') = splitAt i ys
    in ((i,j-i),(zipWith (++) xs ys',ys''))

bjorklund' :: STEP a -> STEP a
bjorklund' (n,x) =
    let (i,j) = n
    in if min i j <= 1
       then (n,x)
       else bjorklund' (if i > j then left (n,x) else right (n,x))

-- | Bjorklund's algorithm to construct a binary sequence of /n/ bits
-- with /k/ ones such that the /k/ ones are distributed as evenly as
-- possible among the (/n/ - /k/) zeroes.
--
-- > bjorklund (5,9) == [True,False,True,False,True,False,True,False,True]
-- > xdot (bjorklund (5,9)) == "x.x.x.x.x"
--
-- > let es = [(2,3),(2,5)
-- >          ,(3,4),(3,5),(3,8)
-- >          ,(4,7),(4,9),(4,12),(4,15)
-- >          ,(5,6),(5,7),(5,8),(5,9),(5,11),(5,12),(5,13),(5,16)
-- >          ,(6,7),(6,13)
-- >          ,(7,8),(7,9),(7,10),(7,12),(7,15),(7,16),(7,17),(7,18)
-- >          ,(8,17),(8,19)
-- >          ,(9,14),(9,16),(9,22),(9,23)
-- >          ,(11,12),(11,24)
-- >          ,(13,24)
-- >          ,(15,34)]
-- > in map (\e -> let e' = bjorklund e in (e,xdot e',iseq_str e')) es
--
-- > [((2,3),"xx.","(12)")
-- > ,((2,5),"x.x..","(23)")
-- > ,((3,4),"xxx.","(112)")
-- > ,((3,5),"x.x.x","(221)")
-- > ,((3,8),"x..x..x.","(332)")
-- > ,((4,7),"x.x.x.x","(2221)")
-- > ,((4,9),"x.x.x.x..","(2223)")
-- > ,((4,12),"x..x..x..x..","(3333)")
-- > ,((4,15),"x...x...x...x..","(4443)")
-- > ,((5,6),"xxxxx.","(11112)")
-- > ,((5,7),"x.xx.xx","(21211)")
-- > ,((5,8),"x.xx.xx.","(21212)")
-- > ,((5,9),"x.x.x.x.x","(22221)")
-- > ,((5,11),"x.x.x.x.x..","(22223)")
-- > ,((5,12),"x..x.x..x.x.","(32322)")
-- > ,((5,13),"x..x.x..x.x..","(32323)")
-- > ,((5,16),"x..x..x..x..x...","(33334)")
-- > ,((6,7),"xxxxxx.","(111112)")
-- > ,((6,13),"x.x.x.x.x.x..","(222223)")
-- > ,((7,8),"xxxxxxx.","(1111112)")
-- > ,((7,9),"x.xxx.xxx","(2112111)")
-- > ,((7,10),"x.xx.xx.xx","(2121211)")
-- > ,((7,12),"x.xx.x.xx.x.","(2122122)")
-- > ,((7,15),"x.x.x.x.x.x.x..","(2222223)")
-- > ,((7,16),"x..x.x.x..x.x.x.","(3223222)")
-- > ,((7,17),"x..x.x..x.x..x.x.","(3232322)")
-- > ,((7,18),"x..x.x..x.x..x.x..","(3232323)")
-- > ,((8,17),"x.x.x.x.x.x.x.x..","(22222223)")
-- > ,((8,19),"x..x.x.x..x.x.x..x.","(32232232)")
-- > ,((9,14),"x.xx.xx.xx.xx.","(212121212)")
-- > ,((9,16),"x.xx.x.x.xx.x.x.","(212221222)")
-- > ,((9,22),"x..x.x..x.x..x.x..x.x.","(323232322)")
-- > ,((9,23),"x..x.x..x.x..x.x..x.x..","(323232323)")
-- > ,((11,12),"xxxxxxxxxxx.","(11111111112)")
-- > ,((11,24),"x..x.x.x.x.x..x.x.x.x.x.","(32222322222)")
-- > ,((13,24),"x.xx.x.x.x.x.xx.x.x.x.x.","(2122222122222)")
-- > ,((15,34),"x..x.x.x.x..x.x.x.x..x.x.x.x..x.x.","(322232223222322)")]
bjorklund :: (Int,Int) -> [Bool]
bjorklund (i,j') =
    let j = j' - i
        x = replicate i [True]
        y = replicate j [False]
        (_,(x',y')) = bjorklund' ((i,j),(x,y))
    in concat x' ++ concat y'

-- | /xdot/ notation for pattern.
--
-- > xdot (bjorklund (5,9)) == "x.x.x.x.x"
xdot :: [Bool] -> String
xdot = map (\x -> if x then 'x' else '.')

-- | The 'iseq' of a pattern is the distance between 'True' values.
--
-- > iseq (bjorklund (5,9)) == [2,2,2,2,1]
iseq :: [Bool] -> [Int]
iseq =
    let f = split . keepDelimsL . whenElt
    in tail . map length . f (== True)

-- | 'iseq' of pattern as compact string.
--
-- > iseq_str (bjorklund (5,9)) == "(22221)"
iseq_str :: [Bool] -> String
iseq_str = let f xs = "(" ++ concatMap show xs ++ ")"
           in f . iseq
