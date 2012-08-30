-- | Polansky, Larry and Bassein, Richard
-- \"Possible and Impossible Melody: Some Formal Aspects of Contour\"
-- /Journal of Music Theory/ 36/2, 1992 (pp.259-284)
-- (<http://www.jstor.org/pss/843933>)
module Music.Theory.Contour.Polansky_1992 where

import Data.List
import Data.List.Split {- split -}
import qualified Data.Map as M {- containers -}
import Data.Maybe
import Data.Ratio
import qualified Music.Theory.Set.List as S
import qualified Music.Theory.Permutations.List as P

-- * List functions

-- | Replace the /i/th value at /ns/ with /x/.
--
-- > replace "test" 2 'n' == "tent"
replace :: Integral i => [a] -> i -> a -> [a]
replace ns i x =
    let f j y = if i == j then x else y
    in zipWith f [0..] ns

-- | Are all elements equal.
--
-- > all_equal "aaa" == True
all_equal :: Eq a => [a] -> Bool
all_equal xs = all id (zipWith (==) xs (tail xs))

-- * Indices

-- | Compare adjacent elements (p.262) left to right.
--
-- > compare_adjacent [0,1,3,2] == [LT,LT,GT]
compare_adjacent :: Ord a => [a] -> [Ordering]
compare_adjacent xs = zipWith compare xs (tail xs)

-- | Construct set of /n/ '-' @1@ adjacent indices, left right order.
--
-- > adjacent_indices 5 == [(0,1),(1,2),(2,3),(3,4)]
adjacent_indices :: Integral i => i -> [(i,i)]
adjacent_indices n = zip [0..n-2] [1..n-1]

-- | All /(i,j)/ indices, in half matrix order.
--
-- > all_indices 4 == [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]
all_indices :: Integral i => i -> [(i,i)]
all_indices n =
    let n' = n - 1
    in [(i,j) | i <- [0 .. n'], j <- [i + 1 .. n']]

-- * 'Enum' functions

-- | Generic variant of 'fromEnum' (p.263).
genericFromEnum :: (Integral i,Enum e) => e -> i
genericFromEnum = fromIntegral . fromEnum

-- | Generic variant of 'toEnum' (p.263).
genericToEnum :: (Integral i,Enum e) => i -> e
genericToEnum = toEnum . fromIntegral

-- * 'Ordering' functions

-- | Specialised 'genericFromEnum'.
ord_to_int :: Integral a => Ordering -> a
ord_to_int = genericFromEnum

-- | Specialised 'genericToEnum'.
int_to_ord :: Integral a => a -> Ordering
int_to_ord = genericToEnum

-- | Invert 'Ordering'.
--
-- > map ord_invert [LT,EQ,GT] == [GT,EQ,LT]
ord_invert :: Ordering -> Ordering
ord_invert x =
    case x of
      LT -> GT
      EQ -> EQ
      GT -> LT

-- * Matrix

-- | A list notation for matrices.
type Matrix a = [[a]]

-- | Apply /f/ to construct 'Matrix' from sequence.
--
-- > matrix_f (,) [1..3] == [[(1,1),(1,2),(1,3)]
-- >                        ,[(2,1),(2,2),(2,3)]
-- >                        ,[(3,1),(3,2),(3,3)]]
matrix_f :: (a -> a -> b) -> [a] -> Matrix b
matrix_f f =
    let g (x,xs) = map (f x) xs
        h xs = map (\x -> (x,xs)) xs
    in map g . h

-- | Construct 'matrix_f' with 'compare' (p.263).
--
-- > contour_matrix [1..3] == [[EQ,LT,LT],[GT,EQ,LT],[GT,GT,EQ]]
contour_matrix :: Ord a => [a] -> Matrix Ordering
contour_matrix = matrix_f compare

-- * Half matrix

-- | Half matrix notation for contour.
data Contour_Half_Matrix =
    Contour_Half_Matrix {contour_half_matrix_n :: Int
                        ,contour_half_matrix_m :: Matrix Ordering}
    deriving (Eq)

-- | Half 'Matrix' of contour given comparison function /f/.
--
-- > half_matrix_f (flip (-)) [2,10,6,7] == [[8,4,5],[-4,-3],[1]]
-- > half_matrix_f (flip (-)) [5,0,3,2] == [[-5,-2,-3],[3,2],[-1]]
-- > half_matrix_f compare [5,0,3,2] == [[GT,GT,GT],[LT,LT],[GT]]
half_matrix_f :: (a -> a -> b) -> [a] -> Matrix b
half_matrix_f f xs =
    let drop_last = reverse . drop 1 . reverse
        m = drop_last (matrix_f f  xs)
    in zipWith drop [1..] m

-- | Construct 'Contour_Half_Matrix' (p.264)
contour_half_matrix :: Ord a => [a] -> Contour_Half_Matrix
contour_half_matrix xs =
    let hm = half_matrix_f compare xs
    in Contour_Half_Matrix (length xs) hm

-- | 'Show' function for 'Contour_Half_Matrix'.
contour_half_matrix_str :: Contour_Half_Matrix -> String
contour_half_matrix_str (Contour_Half_Matrix _ hm) =
    let hm' = map (concatMap (show . fromEnum)) hm
    in unwords hm'

instance Show Contour_Half_Matrix where
    show = contour_half_matrix_str

-- * Contour description

-- | /Description/ notation of contour.
data Contour_Description =
    Contour_Description {contour_description_n :: Int
                        ,contour_description_m :: M.Map (Int,Int) Ordering}
    deriving (Eq)

-- | Construct 'Contour_Description' of contour (p.264).
--
-- > let c = [[3,2,4,1],[3,2,1,4]]
-- > in map (show.contour_description) c == ["202 02 2","220 20 0"]
contour_description :: Ord a => [a] -> Contour_Description
contour_description x =
    let n = length x
        ix = all_indices n
        o = zip ix (map (\(i,j) -> compare (x !! i) (x !! j)) ix)
    in Contour_Description n (M.fromList o)

-- | 'Show' function for 'Contour_Description' (p.264).
contour_description_str :: Contour_Description -> String
contour_description_str (Contour_Description n m) =
    let xs = concatMap (show . fromEnum . snd) (M.toList m)
    in unwords (splitPlaces [n-1,n-2 .. 0] xs)

instance Show Contour_Description where
    show = contour_description_str

-- | Convert from 'Contour_Half_Matrix' notation to 'Contour_Description'.
half_matrix_to_description :: Contour_Half_Matrix -> Contour_Description
half_matrix_to_description (Contour_Half_Matrix n hm) =
    let ix = all_indices n
        o = zip ix (concat hm)
    in Contour_Description n (M.fromList o)

-- | Ordering from /i/th to /j/th element of sequence described at /d/.
--
-- > contour_description_ix (contour_description "abdc") (0,3) == LT
contour_description_ix :: Contour_Description -> (Int,Int) -> Ordering
contour_description_ix d i = contour_description_m d M.! i

-- | 'True' if contour is all descending, equal or ascending.
--
-- > let c = ["abc","bbb","cba"]
-- > in map (uniform.contour_description) c == [True,True,True]
uniform :: Contour_Description -> Bool
uniform (Contour_Description _ m) = all_equal (M.elems m)

-- | 'True' if contour does not containt any 'EQ' elements.
--
-- > let c = ["abc","bbb","cba"]
-- > map (no_equalities.contour_description) c == [True,False,True]
no_equalities :: Contour_Description -> Bool
no_equalities (Contour_Description _ m) = EQ `notElem` M.elems m

-- | Set of all contour descriptions.
--
-- > map (length.all_contours) [3,4,5] == [27,729,59049]
all_contours :: Int -> [Contour_Description]
all_contours n =
    let n' = contour_description_lm n
        ix = all_indices n
        cs = filter (not.null) (S.powerset [LT,EQ,GT])
        pf = concatMap P.multiset_permutations . S.expand_set n'
        mk p = Contour_Description n (M.fromList (zip ix p))
    in map mk (concatMap pf cs)

-- | A sequence of orderings /(i,j)/ and /(j,k)/ may imply ordering
-- for /(i,k)/.
--
-- > map implication [(LT,EQ),(EQ,EQ),(EQ,GT)] == [Just LT,Just EQ,Just GT]
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

-- | List of all violations at a 'Contour_Description' (p.266).
violations :: Contour_Description -> [(Int,Int,Int,Ordering)]
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

-- | Is the number of 'violations' zero.
is_possible :: Contour_Description -> Bool
is_possible = null . violations

-- | All possible contour descriptions
--
-- > map (length.possible_contours) [3,4,5] == [13,75,541]
possible_contours :: Int -> [Contour_Description]
possible_contours = filter is_possible . all_contours

-- | All impossible contour descriptions
--
-- > map (length.impossible_contours) [3,4,5] == [14,654,58508]
impossible_contours :: Int -> [Contour_Description]
impossible_contours = filter (not.is_possible) . all_contours

-- | Calculate number of contours of indicated degree (p.263).
--
-- > map contour_description_lm [2..7] == [1,3,6,10,15,21]
--
-- > let r = [3,27,729,59049,14348907]
-- > in map (\n -> 3 ^ n) (map contour_description_lm [2..6]) == r
contour_description_lm :: Integral a => a -> a
contour_description_lm l = (l * l - l) `div` 2

-- | Truncate a 'Contour_Description' to have at most /n/ elements.
--
-- > let c = contour_description [3,2,4,1]
-- > in contour_truncate c 3 == contour_description [3,2,4]
contour_truncate :: Contour_Description -> Int -> Contour_Description
contour_truncate (Contour_Description n m) z =
    let n' = min n z
        f (i,j) _ = i < n' && j < n'
    in Contour_Description n' (M.filterWithKey f m)

-- | Is 'Contour_Description' /p/ a prefix of /q/.
--
-- > let {c = contour_description [3,2,4,1]
-- >     ;d = contour_description [3,2,4]}
-- > in d `contour_is_prefix_of` c == True
contour_is_prefix_of :: Contour_Description -> Contour_Description -> Bool
contour_is_prefix_of p q = p == contour_truncate q (contour_description_n p)

-- | Are 'Contour_Description's /p/ and /q/ equal at column /n/.
--
-- > let {c = contour_description [3,2,4,1,5]
-- >     ;d = contour_description [3,2,4,1]}
-- > in map (contour_eq_at c d) [0..4] == [True,True,True,True,False]
contour_eq_at :: Contour_Description -> Contour_Description -> Int -> Bool
contour_eq_at p q n =
    let a = contour_description_m p
        b = contour_description_m q
        f (_,j) _ = j == n
        g = M.toAscList . M.filterWithKey f
    in g a == g b

-- * Contour drawing

-- | Derive an 'Integral' contour that would be described by
-- 'Contour_Description'.  Diverges for impossible contours.
--
-- > draw_contour (contour_description "abdc") == [0,1,3,2]
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
                                            LT -> i' + adjustment j'
                                            EQ -> i'
                                            GT -> i' - adjustment j'
                                in Just (replace ns j j'')
        refine [] ns = ns
        refine (i:is) ns = case step i ns of
                             Nothing -> refine is ns
                             Just ns' -> refine ix ns'
    in normalise (refine ix (replicate n 0))

-- | Invert 'Contour_Description'.
--
-- > let c = contour_description "abdc"
-- > in draw_contour (contour_description_invert c) == [3,2,0,1]
contour_description_invert :: Contour_Description -> Contour_Description
contour_description_invert (Contour_Description n m) =
    Contour_Description n (M.map ord_invert m)

-- * Construction

-- | Function to perhaps generate an element and a new state from an
-- initial state.  This is the function provided to 'unfoldr'.
type Build_f st e = st -> Maybe (e,st)

-- | Function to test is a partial sequence conforms to the target
-- sequence.
type Conforms_f e = Int -> [e] -> Bool

-- | Transform a 'Build_f' to produce at most /n/ elements.
--
-- > let f i = Just (i,succ i)
-- > in unfoldr (build_f_n f) (5,'a') == "abcde"
build_f_n :: Build_f st e -> Build_f (Int,st) e
build_f_n f =
    let g g_st =
            let (i,f_st) = g_st
            in if i == 0
               then Nothing
               else case f f_st of
                      Nothing -> Nothing
                      Just (e,f_st') -> Just (e,(i - 1,f_st'))
    in g

-- | Attempt to construct a sequence of /n/ elements given a 'Build_f'
-- to generate possible elements, a 'Conforms_f' that the result
-- sequence must conform to at each step, an 'Int' to specify the
-- maximum number of elements to generate when searching for a
-- solution, and an initial state.
--
-- > let {b_f i = Just (i,i+1)
-- >     ;c_f i x = odd (sum x `div` i)}
-- > in build_sequence 6 b_f c_f 20 0 == (Just [1,2,6,11,15,19],20)
build_sequence :: Int -> Build_f st e -> Conforms_f e -> Int -> st ->
                  (Maybe [e],st)
build_sequence n f g z =
    let go i j r st =
            if i == n
            then (Just r,st)
            else if j == z
                 then (Nothing,st)
                 else case f st of
                        Nothing -> (Nothing,st)
                        Just (e,st') ->
                            let i' = i + 1
                                j' = j + 1
                                r' = r ++ [e]
                            in if g i' r'
                               then go i' j' r' st'
                               else go i j' r st'
    in go 0 0 []

-- | Attempt to construct a sequence that has a specified contour.
-- The arguments are a 'Build_f' to generate possible elements, a
-- 'Contour_Description' that the result sequence must conform to, an
-- 'Int' to specify the maximum number of elements to generate when
-- searching for a solution, and an initial state.
--
-- > import System.Random
--
-- > let {f = Just . randomR ('a','z')
-- >     ;c = contour_description "atdez"
-- >     ;st = mkStdGen 2347}
-- > in fst (build_contour f c 1024 st) == Just "nvruy"
build_contour :: (Ord e) =>
                 Build_f st e -> Contour_Description -> Int -> st ->
                 (Maybe [e],st)
build_contour f c z =
    let n = contour_description_n c
        g i r = let d = contour_description r -- traceShow r
                in contour_eq_at c d (i - 1)
    in build_sequence n f g z

-- | A variant on 'build_contour' that retries a specified number of
-- times using the final state of the failed attempt as the state for
-- the next try.
--
-- > let {f = Just . randomR ('a','z')
-- >     ;c = contour_description "atdezjh"
-- >     ;st = mkStdGen 2347}
-- > in fst (build_contour_retry f c 64 8 st) == Just "nystzvu"
build_contour_retry ::
    (Ord e) =>
    Build_f st e -> Contour_Description -> Int -> Int -> st ->
    (Maybe [e], st)
build_contour_retry f c z n st =
   if n == 0
   then (Nothing,st)
   else case build_contour f c z st of
          (Nothing,st') -> build_contour_retry f c z (n - 1) st'
          r -> r

-- | A variant on 'build_contour_retry' that returns the set of all
-- sequences constructed.
--
-- > let {f = Just . randomR ('a','z')
-- >     ;c = contour_description "atdezjh"
-- >     ;st = mkStdGen 2347}
-- > in length (build_contour_set f c 64 64 st) == 60
build_contour_set ::
    (Ord e) =>
    Build_f st e -> Contour_Description -> Int -> Int -> st -> [[e]]
build_contour_set f c z n st =
    case build_contour_retry f c z n st of
      (Nothing,_) -> []
      (Just r,st') -> r : build_contour_set f c z n st'

-- | Variant of 'build_contour_set' that halts when an generated
-- sequence is a duplicate of an already generated sequence.
--
-- > let {f = randomR ('a','f')
-- >     ;c = contour_description "cafe"
-- >     ;st = mkStdGen 2346836
-- >     ;r = build_contour_set_nodup f c 64 64 st}
-- > in filter ("c" `isPrefixOf`) r == ["cafe","cbed","caed"]
build_contour_set_nodup ::
    Ord e =>
    Build_f st e -> Contour_Description -> Int -> Int -> st -> [[e]]
build_contour_set_nodup f c z n =
    let go r st =
            case build_contour_retry f c z n st of
              (Nothing,_) -> []
              (Just r',st') -> if r' `elem` r
                               then r
                               else go (r' : r) st'
    in go []

-- * Examples

-- | Example from p.262 (quarter-note durations)
--
-- > ex_1 == [2,3/2,1/2,1,2]
-- > compare_adjacent ex_1 == [GT,GT,LT,LT]
-- > show (contour_half_matrix ex_1) == "2221 220 00 0"
-- > draw_contour (contour_description ex_1) == [3,2,0,1,3]
--
-- > let d = contour_description_invert (contour_description ex_1)
-- > in (show d,is_possible d) == ("0001 002 22 2",True)
ex_1 :: [Rational]
ex_1 = [2,3%2,1%2,1,2]

-- | Example on p.265 (pitch)
--
-- > ex_2 == [0,5,3]
-- > show (contour_description ex_2) == "00 2"
ex_2 :: [Integer]
ex_2 = [0,5,3]

-- | Example on p.265 (pitch)
--
-- > ex_3 == [12,7,6,7,8,7]
-- > show (contour_description ex_3) == "22222 2101 000 01 2"
-- > contour_description_ix (contour_description ex_3) (0,5) == GT
-- > is_possible (contour_description ex_3) == True
ex_3 :: [Integer]
ex_3 = [12,7,6,7,8,7]

-- | Example on p.266 (impossible)
--
-- > show ex_4 == "2221 220 00 1"
-- > is_possible ex_4 == False
-- > violations ex_4 == [(0,3,4,GT),(1,3,4,GT)]
ex_4 :: Contour_Description
ex_4 =
    let ns :: [[Int]]
        ns = [[2,2,2,1],[2,2,0],[0,0],[1]]
        ns' = map (map int_to_ord) ns
    in half_matrix_to_description (Contour_Half_Matrix 5 ns')
