-- | Place notation (method ringing).
--
-- Morris, R. G. T. "Place Notation"
-- Central Council of Church Bell Ringers (1984).
-- <http://www.cccbr.org.uk/bibliography/>
module Music.Theory.Permutations.Morris_1984 where

import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Permutations as T {- hmt -}

-- | A change either swaps all adjacent bells, or holds a subset of bells.
data Change = Swap_All | Hold [Int] deriving (Eq,Show)

-- | A method is a sequence of changes, if symmetrical only have the
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

-- | Separate changes.
--
-- > split_changes "-38-14-1258-36-14-58-16-78"
-- > split_changes "345.145.5.1.345" == ["345","145","5","1","345"]
split_changes :: String -> [String]
split_changes = filter (/= ".") . split (dropInitBlank (oneOf "-x."))

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

-- | Parse abbreviated 'Hold' notation, characters are hexedecimal.
--
-- > to_abbrev "38A" == [3,8,10]
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

-- | Given two sequences, derive the one-indexed "hold" list.
--
-- > derive_holds ("12345","13254") == [1]
derive_holds :: (Eq a,Enum n,Num n) => ([a],[a]) -> [n]
derive_holds (p,q) =
    let f n (i,j) = if i == j then Just n else Nothing
    in catMaybes (zipWith f [1..] (zip p q))

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
swap_abbrev :: Int -> [Int] -> [a] -> [a]
swap_abbrev k a =
    let c = to_zero_indexed (swaps_to_cycles (gen_swaps k a))
        p = T.from_cycles c
    in T.apply_permutation p

-- | Apply a 'Change'.
apply_change :: Int -> Change -> [a] -> [a]
apply_change k p l =
    case p of
      Swap_All -> swap_all l
      Hold q -> swap_abbrev k q l

-- | Apply a 'Method', gives next starting sequence and the course of
-- the method.
--
-- > let r = ([1,2,4,5,3]
-- >         ,[[1,2,3,4,5],[2,1,3,4,5],[2,3,1,4,5],[3,2,4,1,5],[3,4,2,5,1]
-- >          ,[4,3,2,5,1],[4,2,3,1,5],[2,4,1,3,5],[2,1,4,3,5],[1,2,4,3,5]])
-- > in apply_method cambridgeshire_slow_course_doubles [1..5] == r
apply_method :: Method -> [a] -> ([a],[[a]])
apply_method m l =
    let k = length l
        f z e = (apply_change k e z,z)
    in mapAccumL f l (method_changes m)

-- | Iteratively apply a 'Method' until it closes (ie. arrives back at
-- the starting sequence).
--
-- > length (closed_method cambridgeshire_slow_course_doubles [1..5]) == 3
closed_method :: Eq a => Method -> [a] -> [[[a]]]
closed_method m l =
    let rec c r =
            let (e,z) = apply_method m c
            in if e == l
               then reverse (z : r)
               else rec e (z : r)
    in rec l []

-- | 'concat' of 'closed_method' with initial sequence appended.
closed_method' :: Eq a => Method -> [a] -> [[a]]
closed_method' m l = concat (closed_method m l) ++ [l]

-- * Methods

-- | Cambridgeshire Slow Course Doubles.
--
-- <https://rsw.me.uk/blueline/methods/view/Cambridgeshire_Slow_Course_Doubles>
--
-- > length (closed_method cambridgeshire_slow_course_doubles [1..5]) == 3
cambridgeshire_slow_course_doubles :: Method
cambridgeshire_slow_course_doubles =
    let a = ("345.145.5.1.345",Just "123")
    in parse_method a

-- | Double Cambridge Cyclic Bob Minor.
--
-- <https://rsw.me.uk/blueline/methods/view/Double_Cambridge_Cyclic_Bob_Minor>
--
-- > length (closed_method double_cambridge_cyclic_bob_minor [1..6]) == 5
double_cambridge_cyclic_bob_minor :: Method
double_cambridge_cyclic_bob_minor =
    let a = ("-14-16-56-36-16-12",Nothing)
    in parse_method a

-- | Hammersmith Bob Triples
--
-- <https://rsw.me.uk/blueline/methods/view/Hammersmith_Bob_Triples>
--
-- > length (closed_method hammersmith_bob_triples [1..7]) == 6
hammersmith_bob_triples :: Method
hammersmith_bob_triples =
    let a = ("7.1.5.123.7.345.7",Just "127")
    in parse_method a

-- | Cambridge Surprise Major.
--
-- <https://rsw.me.uk/blueline/methods/view/Cambridge_Surprise_Major>
--
-- > length (closed_method cambridge_surprise_major [1..8]) == 7
cambridge_surprise_major :: Method
cambridge_surprise_major =
    let a = ("-38-14-1258-36-14-58-16-78",Just "12")
    in parse_method a

-- | Smithsonian Surprise Royal.
--
-- <https://rsw.me.uk/blueline/methods/view/Smithsonian_Surprise_Royal>
--
-- > length (closed_method smithsonian_surprise_royal [1..10]) == 9
smithsonian_surprise_royal :: Method
smithsonian_surprise_royal =
    let a = ("-3A-14-5A-16-347A-18-1456-5A-16-7A",Just "12")
    in parse_method a
