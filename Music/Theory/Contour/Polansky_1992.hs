module Music.Theory.Contour.Polansky_1992 where

{-
Polansky, Larry and Bassein, Richard
"Possible and Impossible Melody: Some Formal Aspects of Contour"
JMT 36/2, 1992 (pp.259-284)
-}

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Music.Theory as T
import qualified Music.Theory.Permutations as T

-- p.262
compare_adjacent :: Ord a => [a] -> [Ordering]
compare_adjacent xs = zipWith compare xs (tail xs)

-- p.263
contour_matrix :: Ord a => [a] -> [[Ordering]]
contour_matrix =
    let fn1 (x,xs) = map (\x' -> compare x x') xs
        fn0 xs = map (\x -> (x,xs)) xs
    in map fn1 . fn0

data Contour_Half_Matrix = Contour_Half_Matrix {
      contour_half_matrix_n :: Int
    , contour_half_matrix_m :: [[Ordering]] } deriving (Eq)

-- p.264
contour_half_matrix :: Ord a => [a] -> Contour_Half_Matrix
contour_half_matrix xs =
    let drop_last = reverse . drop 1 . reverse
        m = drop_last (contour_matrix xs)
        hm = map (\(i,ns) -> drop i ns) (zip [1..] m)
    in Contour_Half_Matrix (length xs) hm

contour_half_matrix_str :: Contour_Half_Matrix -> String
contour_half_matrix_str (Contour_Half_Matrix _ hm) =
    let hm' = map (concatMap (show . fromEnum)) hm
    in intercalate " " hm'

instance Show Contour_Half_Matrix where
    show = contour_half_matrix_str

-- p.263
ord_to_int :: Integral a => Ordering -> a
ord_to_int = fromIntegral . fromEnum

-- p.263
int_to_ord :: Integral a => a -> Ordering
int_to_ord = toEnum . fromIntegral

data Contour_Description = Contour_Description {
      contour_description_n :: Int
    , contour_description_m :: M.Map (Int,Int) Ordering } deriving (Eq)

adjacent_indices :: Integral i => i -> [(i,i)]
adjacent_indices n = zip [0..n-2] [1..n-1]

-- in (i,j) indices in half matrix order
all_indices :: Integral i => i -> [(i,i)]
all_indices n =
    let n' = n - 1
    in [(i,j) | i <- [0 .. n'], j <- [i + 1 .. n']]

-- p.264
contour_description :: Ord a => [a] -> Contour_Description
contour_description x =
    let n = length x
        ix = all_indices n
        o = zip ix (map (\(i,j) -> compare (x !! i) (x !! j)) ix)
    in Contour_Description n (M.fromList o)

-- p.264
contour_description_str :: Contour_Description -> String
contour_description_str (Contour_Description n m) =
    let xs = concatMap (show . fromEnum . snd) (M.toList m)
    in intercalate " " (splitPlaces [n-1,n-2 .. 0] xs)

instance Show Contour_Description where
    show = contour_description_str

half_matrix_to_description :: Contour_Half_Matrix -> Contour_Description
half_matrix_to_description (Contour_Half_Matrix n hm) =
    let ix = all_indices n
        o = zip ix (concat hm)
    in Contour_Description n (M.fromList o)

-- ordering from i-th to j-th element of sequence described at d
contour_description_ix :: Contour_Description -> (Int,Int) -> Ordering
contour_description_ix d i = contour_description_m d M.! i

all_equal :: Eq a => [a] -> Bool
all_equal xs = all id (zipWith (==) xs (tail xs))

-- | true if contour is all descending, equal or ascending
uniform :: Contour_Description -> Bool
uniform (Contour_Description _ m) = all_equal (M.elems m)

-- | true if contour does not containt any EQ elements
no_equalities :: Contour_Description -> Bool
no_equalities (Contour_Description _ m) = not (EQ `elem` M.elems m)

-- | all contour descriptions
all_contours :: Int -> [Contour_Description]
all_contours n =
    let n' = contour_description_lm n
        ix = all_indices n
        cs = filter (not.null) (T.powerset [LT,EQ,GT])
        ps = concatMap (concatMap T.multiset_permutations . T.se n') cs
        mk p = Contour_Description n (M.fromList (zip ix p))
    in map mk ps

-- p.266
violations :: Contour_Description -> [(Int, Int, Int, Ordering)]
violations d =
    let n = contour_description_n d - 1
        ms = [(i,j,k) | i <- [0..n], j <- [i + 1 .. n], k <- [j + 1 .. n]]
        ix = contour_description_ix d
        complies (i,j,k) =
            let l = ix (i,j)
                r = ix (j,k)
                b = ix (i,k)
            in case implication (l,r) of
                 Nothing -> Nothing
                 Just x -> if x == b
                           then Nothing
                           else Just (i,j,k,x)
    in mapMaybe complies ms

is_possible :: Contour_Description -> Bool
is_possible = (== 0) . length . violations

-- | all possible contour descriptions
possible_contours :: Int -> [Contour_Description]
possible_contours = filter is_possible . all_contours

-- | all impossible contour descriptions
impossible_contours :: Int -> [Contour_Description]
impossible_contours = filter (not.is_possible) . all_contours

-- p.263
contour_description_lm :: Integral a => a -> a
contour_description_lm l = (l * l - l) `div` 2

-- a sequence of orderings (i,j) & (j,k) may imply ordering for (i,k)
implication :: (Ordering,Ordering) -> Maybe Ordering
implication (i,j) =
    case (min i j,max i j) of
      (LT,LT) -> Just LT
      (LT,EQ) -> Just LT
      (LT,GT) -> Nothing
      (EQ,EQ) -> Just EQ
      (EQ,GT) -> Just GT
      (GT,GT) -> Just GT
      _ -> error "implication"

-- replace the i-th value at ns with x
replace :: Integral i => [a] -> i -> a -> [a]
replace ns i x =
    let fn (j,y) = if i == j then x else y
    in map fn (zip [0..] ns)

-- diverges for impossible contours
draw_contour :: Integral i => Contour_Description -> [i]
draw_contour d =
    let n = contour_description_n d
        ix = all_indices n
        normalise :: Integral i => [Rational] -> [i]
        normalise xs =
            let xs' = nub (sort xs)
            in map (\i -> fromIntegral (fromJust (findIndex (== i) xs'))) xs
        adjustment x = if x == 0 then 1 else 1 % (denominator x * 2)
        step (i,j) ns = let c = contour_description_ix d (i,j)
                            i' = ns !! i
                            j' = ns !! j
                            c' = compare i' j' -- traceShow (i,j,ns) $
                        in if c == c'
                           then Nothing
                           else let j'' = case c of
                                            LT -> i' + (adjustment j')
                                            EQ -> i'
                                            GT -> i' - (adjustment j')
                                in Just (replace ns j j'')
        refine [] ns = ns
        refine (i:is) ns = case step i ns of
                             Nothing -> refine is ns
                             Just ns' -> refine ix ns'
    in normalise (refine ix (replicate n 0))

ord_invert :: Ordering -> Ordering
ord_invert x =
    case x of
      LT -> GT
      EQ -> EQ
      GT -> LT

contour_description_invert :: Contour_Description -> Contour_Description
contour_description_invert (Contour_Description n m) =
    Contour_Description n (M.map ord_invert m)

-- p.262 (quarter-note durations)
ex_1 :: [Rational]
ex_1 = [2,3%2,1%2,1,2]

-- p.265 (pitch)
ex_2 :: [Integer]
ex_2 = [0,5,3]

-- p.265 (pitch)
ex_3 :: [Integer]
ex_3 = [12,7,6,7,8,7]

-- p.266 (impossible)
ex_4 :: Contour_Description
ex_4 =
    let ns :: [[Int]]
        ns = [[2,2,2,1],[2,2,0],[0,0],[1]]
        ns' = map (map int_to_ord) ns
    in half_matrix_to_description (Contour_Half_Matrix 5 ns')
