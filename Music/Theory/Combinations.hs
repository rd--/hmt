-- | Combination functions.
module Music.Theory.Combinations where

import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Permutations as T {- hmt -}

-- | Number of /k/ element combinations of a set of /n/ elements.
--
-- > map (uncurry nk_combinations) [(4,2),(5,3),(6,3),(13,3)] == [6,10,20,286]
nk_combinations :: Integral a => a -> a -> a
nk_combinations n k = T.nk_permutations n k `div` T.factorial k

-- | /k/ element subsets of /s/.
--
-- > combinations 3 [1..4] == [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
-- > length (combinations 3 [1..5]) == nk_combinations 5 3
-- > combinations 3 "xyzw" == ["xyz","xyw","xzw","yzw"]
combinations :: Int -> [a] -> [[a]]
combinations k s =
    case (k,s) of
      (0,_) -> [[]]
      (_,[]) -> []
      (_,e:s') -> map (e :) (combinations (k - 1) s') ++ combinations k s'

-- * Dyck

-- | <http://www.acta.sapientia.ro/acta-info/C1-1/info1-9.pdf> (P.110)
--
-- > dyck_words_lex 3 == [[0,0,0,1,1,1],[0,0,1,0,1,1],[0,0,1,1,0,1],[0,1,0,0,1,1],[0,1,0,1,0,1]]
dyck_words_lex :: (Num t, Ord t) => t -> [[t]]
dyck_words_lex n =
  let gen x i n0 n1 =
        let d0 = gen (x ++ [0]) (i + 1) (n0 + 1) n1
            d1 = gen (x ++ [1]) (i + 1) n0 (n1 + 1)
        in if (n0 < n) && (n1 < n) && (n0 > n1)
        then concat [d0,d1]
        else if ((n0 < n) && (n1 < n) && (n0 == n1)) || ((n0 < n) && (n1 == n))
             then d0
             else if (n0 == n) && (n1 < n)
                  then d1
                  else if (n0 == n1) && (n1 == n)
                       then [x]
                       else error "?"
  in gen [0] (1::Int) 1 0

-- | Translate 01 to [].
--
-- > unwords (map dyck_word_to_str (dyck_words_lex 3)) == "[[[]]] [[][]] [[]][] [][[]] [][][]"
dyck_word_to_str :: Integral n => [n] -> [Char]
dyck_word_to_str = map (\n -> if n == 0 then '[' else if n == 1 then ']' else undefined)

-- | Translate [] to 01
dyck_word_from_str :: Integral n => [Char] -> [n]
dyck_word_from_str = map (\x -> if x == '[' then 0 else if x == ']' then 1 else undefined)

-- | Is /x/ a segment of a lattice word.
is_lattice_segment :: Integral n => [n] -> Bool
is_lattice_segment x =
  let h = T.histogram x
      f (i,j) = case lookup (i + 1) h of
                  Nothing -> True
                  Just k -> j >= k
  in all f h

-- | Is /x/ a lattice word.
--
-- is_lattice_word [1,1,1,2,2,1,2,1] == True
is_lattice_word :: Integral n => [n] -> Bool
is_lattice_word = all is_lattice_segment . inits

-- | 'is_lattice_word' of 'reverse'.
is_yamanouchi_word :: Integral n => [n] -> Bool
is_yamanouchi_word = is_lattice_word . reverse

-- | 'is_lattice_word' of 'dyck_word_from_str'
--
-- > is_dyck_word "[][[][[[][]]]]" == True
is_dyck_word :: String -> Bool
is_dyck_word = is_lattice_word . (dyck_word_from_str :: String -> [Int])
