-- | Place notation (method ringing).
--
-- Morris, R. G. T. "Place Notation"
-- Central Council of Church Bell Ringers (1984).
-- <http://www.cccbr.org.uk/bibliography/>
module Music.Theory.Permutations.Morris_1984 where

import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Permutations as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- | A change either swaps all adjacent bells, or holds a subset of bells.
data Change = Swap_All | Hold [Int] deriving (Eq,Show)

-- | A method is a sequence of changes, if symmetrical only half the
-- changes are given and the lead end.
data Method = Method [Change] (Maybe [Change]) deriving (Eq,Show)

-- | Maximum hold value at 'Method'
method_limit :: Method -> Int
method_limit (Method p q) =
  let f c = case c of
              Swap_All -> 0
              Hold i -> maximum i
  in maximum (map f (p ++ maybe [] id q))

-- | Complete list of 'Change's at 'Method', writing out symmetries.
method_changes :: Method -> [Change]
method_changes (Method p q) =
    case q of
      Nothing -> p
      Just le -> p ++ tail (reverse p) ++ le

-- | Parse a change notation.
--
-- > map parse_change ["-","x","38"] == [Swap_All,Swap_All,Hold [3,8]]
parse_change :: String -> Change
parse_change s = if is_swap_all s then Swap_All else Hold (map nchar_to_int s)

-- | Separate changes.
--
-- > split_changes "-38-14-1258-36-14-58-16-78"
-- > split_changes "345.145.5.1.345" == ["345","145","5","1","345"]
split_changes :: String -> [String]
split_changes = filter (/= ".") . split (dropInitBlank (oneOf "-x."))

-- | Place notation, sequence of changes with possible lead end.
type Place = (String,Maybe String)

-- | Parse 'Method' given 'PLACE' notation.
parse_method :: Place -> Method
parse_method (p,q) =
    let f = map parse_change . split_changes
    in Method (f p) (fmap f q)

-- | Parse string into 'Place'.
--
-- > parse_method (parse_place "-38-14-1258-36-14-58-16-78,12")
parse_place :: String -> Place
parse_place txt =
  case splitOn "," txt of
    [p] -> (p,Nothing)
    [p,q] -> (p,Just q)
    _ -> error "parse_place?"

-- | - or x?
--
-- > map is_swap_all ["-","x","38"] == [True,True,False]
is_swap_all :: String -> Bool
is_swap_all = flip elem ["-","x"]

-- | Flatten list of pairs.
--
-- > flatten_pairs [(1,2),(3,4)] == [1..4]
flatten_pairs :: [(a,a)] -> [a]
flatten_pairs = concatMap T.t2_to_list

-- | Swap all adjacent pairs at list.
--
-- > swap_all [1 .. 8] == [2,1,4,3,6,5,8,7]
swap_all :: [a] -> [a]
swap_all = flatten_pairs . map T.p2_swap . T.adj2 2

numeric_spelling_tbl :: [(Char,Int)]
numeric_spelling_tbl = zip "1234567890ETABCDFGHJKL" [1 .. 22]

-- | Parse abbreviated 'Hold' notation, characters are NOT hexadecimal.
--
-- > map nchar_to_int "380ETA" == [3,8,10,11,12,13]
nchar_to_int :: Char -> Int
nchar_to_int = fromMaybe (error "nchar_to_int") . flip lookup numeric_spelling_tbl

-- | Inverse of 'nchar_to_int'.
--
-- > map int_to_nchar [3,8,10,11,12,13] == "380ETA"
int_to_nchar :: Int -> Char
int_to_nchar = flip T.reverse_lookup_err numeric_spelling_tbl

-- | Given a 'Hold' notation, generate permutation cycles.
--
-- > let r = [Right (1,2),Left 3,Right (4,5),Right (6,7),Left 8]
-- > gen_swaps 8 [3,8] == r
--
-- > r = [Left 1,Left 2,Right (3,4),Right (5,6),Right (7,8)]
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
-- > to_zero_indexed [[1],[2],[3,4],[5,6],[7,8]] == r
to_zero_indexed :: Enum t => [[t]] -> [[t]]
to_zero_indexed = map (map pred)

-- | Apply abbreviated 'Hold' notation, given cardinality.
--
-- > swap_abbrev 8 [3,8] [2,1,4,3,6,5,8,7] == [1,2,4,6,3,8,5,7]
swap_abbrev :: Int -> [Int] -> [a] -> [a]
swap_abbrev k a =
    let c = to_zero_indexed (swaps_to_cycles (gen_swaps k a))
        p = T.from_cycles_zero_indexed c
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
-- > apply_method cambridgeshire_slow_course_doubles [1..5] == r
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
closed_method_lp :: Eq a => Method -> [a] -> [[a]]
closed_method_lp m l = concat (closed_method m l) ++ [l]

-- | 'closed_method' of 'parse_method'
closed_place :: Eq t => Place -> [t] -> [[[t]]]
closed_place pl l = closed_method (parse_method pl) l

-- * Methods

-- | <https://rsw.me.uk/blueline/methods/view/Cambridgeshire_Place_Doubles>
--
-- > length (closed_place cambridgeshire_place_doubles_pl [1..5]) == 3
cambridgeshire_place_doubles_pl :: Place
cambridgeshire_place_doubles_pl = ("345.145.5.1.345",Just "123")

-- | 'parse_method' of 'cambridgeshire_place_doubles_pl'
cambridgeshire_slow_course_doubles :: Method
cambridgeshire_slow_course_doubles = parse_method cambridgeshire_place_doubles_pl

-- | <https://rsw.me.uk/blueline/methods/view/Double_Cambridge_Cyclic_Bob_Minor>
--
-- > length (closed_place double_cambridge_cyclic_bob_minor_pl [1..6]) == 5
double_cambridge_cyclic_bob_minor_pl :: Place
double_cambridge_cyclic_bob_minor_pl = ("-14-16-56-36-16-12",Nothing)

-- | 'parse_method' of 'double_cambridge_cyclic_bob_minor_pl'
double_cambridge_cyclic_bob_minor :: Method
double_cambridge_cyclic_bob_minor = parse_method (double_cambridge_cyclic_bob_minor_pl)

-- | <https://rsw.me.uk/blueline/methods/view/Hammersmith_Bob_Triples>
--
-- > length (closed_place hammersmith_bob_triples_pl [1..7]) == 6
hammersmith_bob_triples_pl :: Place
hammersmith_bob_triples_pl = ("7.1.5.123.7.345.7",Just "127")

hammersmith_bob_triples :: Method
hammersmith_bob_triples = parse_method hammersmith_bob_triples_pl

-- | <https://rsw.me.uk/blueline/methods/view/Cambridge_Surprise_Major>
--
-- > length (closed_place cambridge_surprise_major_pl [1..8]) == 7
cambridge_surprise_major_pl :: Place
cambridge_surprise_major_pl = ("-38-14-1258-36-14-58-16-78",Just "12")

cambridge_surprise_major :: Method
cambridge_surprise_major = parse_method cambridge_surprise_major_pl

-- | <https://rsw.me.uk/blueline/methods/view/Smithsonian_Surprise_Royal>
--
-- > let c = closed_place smithsonian_surprise_royal_pl [1..10]
-- > (length c,nub (map length c),sum (map length c)) == (9,[40],360)
smithsonian_surprise_royal_pl :: Place
smithsonian_surprise_royal_pl = ("-30-14-50-16-3470-18-1456-50-16-70",Just "12")

smithsonian_surprise_royal :: Method
smithsonian_surprise_royal = parse_method smithsonian_surprise_royal_pl

-- | <https://rsw.me.uk/blueline/methods/view/Ecumenical_Surprise_Maximus>
--
-- > c = closed_place ecumenical_surprise_maximus_pl [1..12]
-- > (length c,nub (map length c),sum (map length c)) == (11,[48],528)
ecumenical_surprise_maximus_pl :: Place
ecumenical_surprise_maximus_pl = ("x3Tx14x5Tx16x7Tx1238x149Tx50x16x7Tx18.90.ET",Just "12")

ecumenical_surprise_maximus :: Method
ecumenical_surprise_maximus = parse_method ecumenical_surprise_maximus_pl
