-- | Max Meyer. \"The musician's arithmetic: drill problems for an
-- introduction to the scientific study of musical composition.\" The
-- University of Missouri, 1929.  p.22
module Music.Theory.Tuning.Meyer_1929 where

import Data.List
import Data.Ratio
import qualified Music.Theory.Tuning as T

-- | Odd numbers to /n/.
--
-- > odd_to 7 == [1,3,5,7]
odd_to :: (Num t, Enum t) => t -> [t]
odd_to n = [1,3 .. n]

-- | Generate initial row for /n/.
--
-- > row 7 == [1,5/4,3/2,7/4]
row :: Integral i => i -> [Ratio i]
row = sort . map T.fold_to_octave . odd_to . (% 1)

-- | Generate initial column for /n/.
--
-- > column 7 == [1,8/5,4/3,8/7]
column :: Integral i => i -> [Ratio i]
column = map (T.fold_to_octave . recip) . row

-- | 'T.fold_to_octave' '.' '*'.
in_oct_mul :: Integral i => Ratio i -> Ratio i -> Ratio i
in_oct_mul i j = T.fold_to_octave (i * j)

-- | Given /row/ and /column/ generate matrix value at /(i,j)/.
--
-- > inner (row 7,column 7) (1,2) == 6/5
inner :: Integral i => ([Ratio i],[Ratio i]) -> (i,i) -> Ratio i
inner (r,c) (i,j) = in_oct_mul (r `genericIndex` j) (c `genericIndex` i)

-- | Meyer table in form /(r,c,n)/.
--
-- > meyer_table_indices 7 == [(0,0,1/1),(0,1,5/4),(0,2,3/2),(0,3,7/4)
-- >                          ,(1,0,8/5),(1,1,1/1),(1,2,6/5),(1,3,7/5)
-- >                          ,(2,0,4/3),(2,1,5/3),(2,2,1/1),(2,3,7/6)
-- >                          ,(3,0,8/7),(3,1,10/7),(3,2,12/7),(3,3,1/1)]
meyer_table_indices :: Integral i => i -> [(i,i,Ratio i)]
meyer_table_indices n =
    let r = row n
        c = column n
        k = n `div` 2
    in [(i,j,inner (r,c) (i,j)) | i <- [0..k], j <- [0..k]]

-- | Meyer table as set of rows.
--
-- > meyer_table_rows 7 == [[1/1, 5/4, 3/2,7/4]
-- >                       ,[8/5, 1/1, 6/5,7/5]
-- >                       ,[4/3, 5/3, 1/1,7/6]
-- >                       ,[8/7,10/7,12/7,1/1]]
--
-- > let r = [[ 1/1,   9/8,   5/4,  11/8,   3/2,  13/8,   7/4,  15/8]
-- >         ,[16/9,   1/1,  10/9,  11/9,   4/3,  13/9,  14/9,   5/3]
-- >         ,[ 8/5,   9/5,   1/1,  11/10,  6/5,  13/10,  7/5,   3/2]
-- >         ,[16/11, 18/11, 20/11,  1/1,  12/11, 13/11, 14/11, 15/11]
-- >         ,[ 4/3,   3/2,   5/3,  11/6,   1/1,  13/12,  7/6,   5/4]
-- >         ,[16/13, 18/13, 20/13, 22/13, 24/13,  1/1,  14/13, 15/13]
-- >         ,[ 8/7,   9/7,   10/7, 11/7,  12/7,  13/7,   1/1,  15/14]
-- >         ,[16/15,  6/5,    4/3, 22/15,  8/5,  26/15, 28/15,  1/1]]
-- > in meyer_table_rows 15 == r
meyer_table_rows :: Integral a => a -> [[Ratio a]]
meyer_table_rows n =
    let r = row n
        c = column n
        k = n `div` 2
        rn i = [inner (r,c) (i,j) | j <- [0..k]]
    in map rn [0..k]

-- | Third element of three-tuple.
t3_3 :: (t1,t2,t3) -> t3
t3_3 (_,_,i) = i

-- | Set of unique ratios in /n/ table.
--
-- > elements 7 == [1,8/7,7/6,6/5,5/4,4/3,7/5,10/7,3/2,8/5,5/3,12/7,7/4]
--
-- > elements 9 == [1,10/9,9/8,8/7,7/6,6/5,5/4,9/7,4/3,7/5,10/7
-- >               ,3/2,14/9,8/5,5/3,12/7,7/4,16/9,9/5]
elements :: Integral i => i -> [Ratio i]
elements = nub . sort . concat . meyer_table_rows

-- | Number of unique elements at /n/ table.
--
-- > map degree [7,9,11,13,15] == [13,19,29,41,49]
degree :: Integral i => i -> i
degree = genericLength . elements

-- | <http://en.wikipedia.org/wiki/Farey_sequence>
--
-- > let r = [[0,1/2,1]
-- >         ,[0,1/3,1/2,2/3,1]
-- >         ,[0,1/4,1/3,1/2,2/3,3/4,1]
-- >         ,[0,1/5,1/4,1/3,2/5,1/2,3/5,2/3,3/4,4/5,1]
-- >         ,[0,1/6,1/5,1/4,1/3,2/5,1/2,3/5,2/3,3/4,4/5,5/6,1]]
-- > in map farey_sequence [2..6] == r
farey_sequence :: Integral a => a -> [Ratio a]
farey_sequence k = 0 : nub (sort ([n%d | d <- [1..k], n <- [1..d]]))
