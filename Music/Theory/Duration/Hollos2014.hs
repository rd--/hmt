-- http://abrazol.com/books/rhythm1/software.html
module Music.Theory.Duration.Hollos2014 where

import Data.List {- base -}

import Music.Theory.List {- hmt -}
import Music.Theory.Permutations.List {- hmt -}
import Music.Theory.Set.List {- hmt -}

-- | Donald Knuth, Art of Computer Programming, Algorithm H
--   <http://www-cs-faculty.stanford.edu/~knuth/fasc3b.ps.gz>
--
-- > partm 3 6 == [[1,1,4],[2,1,3],[2,2,2]]
partm :: (Num a, Ord a) => a -> a -> [[a]]
partm i j =
  let f t m n =
        if m == 1 && t == n
        then [[t]]
        else if n < m || n < 1 || m < 1 || t < 1
             then []
        else [reverse (t : r) | r <- f t (m - 1) (n - t)] ++ (f (t - 1) m n)
  in f (j - i + 1) i j

-- | Generates all partitions of n.
--
-- > compUniq 4 == [[1,1,1,1],[1,1,2],[1,3],[2,2],[4]]
-- > compUniq 5 == [[1,1,1,1,1],[1,1,1,2],[1,1,3],[2,1,2],[1,4],[2,3],[5]]
part :: (Num a, Ord a, Enum a) => a -> [[a]]
part n = concatMap (\k -> partm k n) (reverse [1 .. n])

-- | Generates all partitions of n with parts in the set e.
--
-- > parta 8 [2,3] == [[2,2,2,2],[3,2,3]]
parta :: (Num a, Ord a, Enum a) => a -> [a] -> [[a]]
parta n e = filter (all (`elem` e)) (part n)

-- | Generate all compositions of n.
--
-- > comp 4 == [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[1,3],[3,1],[2,2],[4]]
-- > length (comp 8) == 128
comp :: (Num a, Ord a, Enum a) => a -> [[a]]
comp = concatMap multiset_permutations . part

-- | Generates all compositions of n into k parts.
--
-- > compm 3 6 == [[1,1,4],[1,4,1],[4,1,1],[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1],[2,2,2]]
-- > length (compm 5 16) == 1365
compm :: (Ord a, Num a) => a -> a -> [[a]]
compm k = concatMap multiset_permutations . partm k

-- | Generates all compositions of n with parts in the set (p1 p2 ... pk).
--
-- > compa 8 [3,4,5,6] == [[3,5],[5,3],[4,4]]
compa :: (Num a, Ord a, Enum a) => a -> [a] -> [[a]]
compa n e = filter (all (`elem` e)) (comp n)

-- | Generates all compositions of n with m parts in the set (p1 p2 ... pk).
--
-- > compam 4 16 [3,4,5]
compam :: (Num a, Ord a, Enum a) => a -> a -> [a] -> [[a]]
compam k n e = filter (all (`elem` e)) (compm k n)

-- | Generates all binary necklaces of length n.  <http://combos.org/necklace>
--
-- > neck 5 == [[1,1,1,1,1],[1,1,1,1,0],[1,1,0,1,0],[1,1,1,0,0],[1,0,1,0,0],[1,1,0,0,0],[1,0,0,0,0],[0,0,0,0,0]]
neck :: (Ord t, Num t) => Int -> [[t]]
neck n = concatMap multiset_cycles [replicate i 0 ++ replicate (n - i) 1 | i <- [0 .. n]]

-- | Generates all binary necklaces of length n with m ones.
--
-- > neckm 8 2 == [[1,0,0,0,1,0,0,0],[1,0,0,1,0,0,0,0],[1,0,1,0,0,0,0,0],[1,1,0,0,0,0,0,0]]
neckm :: (Num a, Ord a) => Int -> Int -> [[a]]
neckm n m = filter ((== m) . length . filter (== 1)) (neck n)

-- | Part is the length of a substring 10...0 composing the necklace.
--   For example the necklace 10100 has parts of size 2 and 3.
--
-- > necklaceParts [1,0,1,0,0] == [2,3]
-- > necklaceParts [0,0,0,0,0,0,0,0] == []
necklaceParts :: (Eq a, Num a) => [a] -> [Int]
necklaceParts l = d_dx (findIndices (== 1) l ++ [length l])

necklaceWithParts :: (Eq a, Num a) => [Int] -> [a] -> Bool
necklaceWithParts e l =
  let p = necklaceParts l
  in not (null p) && all (`elem` e) p

-- | Generates all binary necklaces of length n with parts in e.
--
-- > necka 8 [2,3,4] == [[1,0,1,0,1,0,1,0],[1,0,1,0,0,1,0,0],[1,0,1,0,1,0,0,0],[1,0,0,0,1,0,0,0]]
necka :: (Num a, Ord a) => Int -> [Int] -> [[a]]
necka n e = filter (necklaceWithParts e) (neck n)

-- | Generates all binary necklaces of length n with m ones and parts in e.
neckam :: (Num a, Ord a) => Int -> Int -> [Int] -> [[a]]
neckam n m e = filter (necklaceWithParts e) (neckm n m)

-- | Generates all permutations of the non-negative integers in the set.
--
-- > permi [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permi :: [a] -> [[a]]
permi = permutations_l
