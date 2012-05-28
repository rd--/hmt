-- | Tom Johnson. \"Tiling in my Music\". /The Experimental Music
-- Yearbook/, 1, 2009.
module Music.Theory.Tiling.Johnson_2009 where

import Data.List
import Data.List.Split {- split -}

type E = ([Int],[(Int,[Int])])
type C = [E]
type V = [[Int]]

e_voices :: E -> V
e_voices (p,q) =
    let f (m,n) = let p' = map (* m) p
                  in map (\i -> map (+ i) p') n
    in return (sort (concat (concatMap f q)))

c_voices :: C -> V
c_voices = concatMap e_voices

v_limit :: V -> Int
v_limit = maximum . concat

v_dot_star :: V -> [String]
v_dot_star v =
    let n = v_limit v
        f p i = if i `elem` p then '*' else '.'
        g p = map (f p) [0..n]
    in map g v

v_dot_star_m :: Int -> V -> [String]
v_dot_star_m m =
    let f = concat . intersperse "|" . splitEvery m
    in map f . v_dot_star

e_no_mul :: ([Int],[Int]) -> [E]
e_no_mul (p,q) =
    let q' = zip (repeat 1) (map return q)
    in map (\i -> (p,[i])) q'

c_no_mul :: [([Int],[Int])] -> C
c_no_mul = concat . map e_no_mul

-- | Print @.*@ diagram for 'C'.
--
-- > c_print (c_no_mul [([0,2,5],[0,1,8]),([0,3,5],[4,11,12])])
-- > c_print (c_no_mul [([0..5],[0,6,12,18])])
-- > c_print (c_no_mul [([0,2,3,4,5,7],[0,6,12,18])])
-- > c_print (c_no_mul [([0,1,3,4,5,8],[0,6,12,18])])
-- > c_print (c_no_mul [([0,3,4,5,7,8],[0,6,12,18])])
c_print :: C -> IO ()
c_print = putStrLn . unlines . ("" :) . v_dot_star . c_voices

-- | Variant to print @|@ at measures.
--
-- > c_print_m (Just 6) (map (\i -> ([0,1,3],[i]))
-- >                         [(1,[0,36..180])
-- >                         ,(2,[7,7+36..180])
-- >                         ,(3,[25,25+36..180])
-- >                         ,(4,[51,51+36..180])
-- >                         ,(5,[52,52+36..180])
-- >                         ,(6,[78,78+36..180])
-- >                         ,(7,[105,105+36..180])
-- >                         ,(8,[130,130+36..180])])
c_print_m :: Maybe Int -> C -> IO ()
c_print_m n =
    let f = maybe v_dot_star v_dot_star_m n
    in putStrLn . unlines . ("":) . f . c_voices
