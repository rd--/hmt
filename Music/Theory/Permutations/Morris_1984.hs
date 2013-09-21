-- | Place notation (method ringing).
--
-- Morris, R. G. T. "Place Notation"
-- Central Council of Church Bell Ringers (1984)
module Music.Theory.Permutations.Morris_1984 where

import Data.Char {- base -}
import Data.List.Split {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Permutations as T {- hmt -}

-- | A change either swaps all adjacent bells, or holds a subset of bells.
data Change = Swap_All | Hold [Int] deriving (Eq,Show)

-- | A method is a sequence of changes, if symetrical only have the
-- changes are given and the lead end.
data Method = Method [Change] (Maybe Change) deriving (Eq,Show)

-- | Compete list of 'Change's at 'Method', writing out symmetries.
method_changes :: Method -> [Change]
method_changes (Method p q) =
    case q of
      Nothing -> p
      Just q' -> p ++ tail (reverse p) ++ [q']

-- | Parse a change notation.
--
-- > map parse_change ["-","x","38"] == [Swap_All,Swap_All,Hold [3,8]]
parse_change :: String -> Change
parse_change s = if is_swap_all s then Swap_All else Hold (to_abbrev s)

-- | Cambridge Surprise Major.
--
-- <https://rsw.me.uk/blueline/methods/view/Cambridge_Surprise_Major>
cambridge_surprise_major :: Method
cambridge_surprise_major =
    let a = ("-38-14-1258-36-14-58-16-78",Just "12")
    in parse_method a

-- | Separate changes.
--
-- > split_changes "-38-14-1258-36-14-58-16-78"
split_changes :: String -> [String]
split_changes = split (dropInitBlank (oneOf "-x"))

-- | Parse 'Method' from the sequence of changes with possible lead end.
--
-- > parse_method ("-38-14-1258-36-14-58-16-78",Just "12")
parse_method :: (String,Maybe String) -> Method
parse_method (p,q) =
    let c = map parse_change (split_changes p)
        le = fmap parse_change q
    in Method c le

-- > map is_swap_all ["-","x","38"] == [True,True,False]
is_swap_all :: String -> Bool
is_swap_all s =
    case s of
      [c] -> c `elem` "-x"
      _ -> False

-- | Swap elemets of two-tuple (pair).
--
-- > swap_pair (1,2) == (2,1)
swap_pair :: (s,t) -> (t,s)
swap_pair (p,q) = (q,p)

-- | Flatten list of pairs.
--
-- > flatten_pairs [(1,2),(3,4)] == [1..4]
flatten_pairs :: [(a,a)] -> [a]
flatten_pairs l =
    case l of
      [] -> []
      (p,q):l' -> p : q : flatten_pairs l'

-- | Swap all adjacent pairs at list.
--
-- > swap_all [1 .. 8] == [2,1,4,3,6,5,8,7]
swap_all :: [a] -> [a]
swap_all = flatten_pairs . map swap_pair . T.adj2 2

-- | Parse abbreviated 'Hold' notation.
--
-- > to_abbrev "38" == [3,8]
to_abbrev :: String -> [Int]
to_abbrev = map digitToInt

-- | Given a 'Hold' notation, generate permutation cycles.
--
-- > let r = [Right (1,2),Left 3,Right (4,5),Right (6,7),Left 8]
-- > in gen_swaps 8 [3,8] == r
--
-- > let r = [Left 1,Left 2,Right (3,4),Right (5,6),Right (7,8)]
-- > gen_swaps 8 [1,2] == r
gen_swaps :: (Num t, Ord t) => t -> [t] -> [Either t (t,t)]
gen_swaps k =
    let close n = if n < k then Right (n,n + 1) : close (n + 2) else []
        rec n l = case l of
                    [] -> close n
                    m:l' -> if n < m
                            then Right (n,n+1) : rec (n + 2) l
                            else Left n : rec (m + 1) l'
    in rec 1

-- | Two-tuple to two element list.
pair_to_list :: (t,t) -> [t]
pair_to_list (p,q) = [p,q]

-- | Swap notation to plain permutation cycles notation.
--
-- > let n = [Left 1,Left 2,Right (3,4),Right (5,6),Right (7,8)]
-- > in swaps_to_cycles n == [[1],[2],[3,4],[5,6],[7,8]]
swaps_to_cycles :: [Either t (t,t)] -> [[t]]
swaps_to_cycles = map (either return pair_to_list)

-- | One-indexed permutation cycles to zero-indexed.
--
-- > let r = [[0],[1],[2,3],[4,5],[6,7]]
-- > in to_zero_indexed [[1],[2],[3,4],[5,6],[7,8]] == r
to_zero_indexed :: Enum t => [[t]] -> [[t]]
to_zero_indexed = map (map pred)

-- | Apply abbreviated 'Hold' notation, given cardinality.
--
-- > swap_abbrev 8 [3,8] [2,1,4,3,6,5,8,7] == [1,2,4,6,3,8,5,7]
swap_abbrev :: Eq a => Int -> [Int] -> [a] -> [a]
swap_abbrev k a =
    let c = to_zero_indexed (swaps_to_cycles (gen_swaps k a))
        p = T.from_cycles c
    in T.apply_permutation p

-- | Apply a 'Change'.
apply_change :: Eq a => Int -> Change -> [a] -> [a]
apply_change k p l =
    case p of
      Swap_All -> swap_all l
      Hold q -> swap_abbrev k q l

-- | Apply a 'Method'.
--
-- > apply_method cambridge_surprise_major [1..8]
apply_method :: Eq a => Method -> [a] -> [[a]]
apply_method m l =
    let k = length l
        rec z p = case p of
                    [] -> []
                    e : p' -> let z' = apply_change k e z
                              in z' : rec z' p'
    in rec l (method_changes m)

-- | Iteratively apply a 'Method' until it closes (ie. arrives back at
-- the starting sequence).
--
-- > length (closed_method cambridge_surprise_major [1..8]) == 7 * 31 + 1
closed_method :: Eq a => Method -> [a] -> [[a]]
closed_method m l =
    let rec c r =
            let z = apply_method m c
                e = last z
            in if e == l
               then concat (reverse (z : r))
               else rec e (T.dropRight 1 z : r)
    in rec l []
