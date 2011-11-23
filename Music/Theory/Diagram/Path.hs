-- | Functions to make /path diagrams/ such as those in Fig. VIII-11
-- on I.Xenakis /Formalized Music/.
module Music.Theory.Diagram.Path where

import Data.CG.Minus {- hcg-minus -}
import Data.Function
import Data.List
import Data.Maybe

-- * Genera

-- | Set of all /(pre,element,post)/ triples of a sequence.
--
-- > parts "abc" == [("",'a',"bc"),("a",'b',"c"),("ab",'c',"")]
parts :: [a] -> [([a],a,[a])]
parts inp =
    let f p i q = let r = (p,i,q)
                  in case q of
                       [] -> [r]
                       (q':q'') -> r : f (p ++ [i]) q' q''
    in case inp of
         (x:xs) -> f [] x xs
         [] -> []

-- | All /(element,remainder)/ pairs for a sequence.
--
-- > parts' "abc" == [('a',"bc"),('b',"ac"),('c',"ab")]
parts' :: [a] -> [(a,[a])]
parts' = let f (p,i,q) = (i,p++q) in map f . parts

-- | Gather elements with equal keys.
--
-- > gather (zip "abcba" [0..]) == [('a',[0,4]),('b',[1,3]),('c',[2])]
gather :: (Ord a) => [(a,i)] -> [(a,[i])]
gather =
    let f xs = (fst (head xs),map snd xs)
    in map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- * Geometry

-- | Does either endpoint of the /lhs/ 'Ln' lie on the /rhs/ 'Ln'.
--
-- > ln_on (ln' (1/2,1/2) (1/2,1)) (ln' (0,0) (1,1)) == True
-- > ln_on (ln' (1/2,0) (1/2,1)) (ln' (0,0) (1,1)) == False
ln_on :: Ln R -> Ln R -> Bool
ln_on l0 l1 =
    let (p,q) = ln_pt l0
    in pt_on_line l1 p || pt_on_line l1 q

-- | Do 'Ln's overlap in the particular sense of being 'ln_parallel'
-- and at least one endpoint of one line lying on the other.
overlap :: Ln R -> Ln R -> Bool
overlap p q = ln_parallel p q && (ln_on p q || ln_on q p)

-- | Do both points of the /rhs/ 'Ln' lie on the /lhs/ 'Ln'.
includes :: Ln R -> Ln R -> Bool
includes l0 l1 =
    let (p,q) = ln_pt l1
        f = pt_on_line l0
    in f p && f q

-- | 'flip' 'includes'.
is_included :: Ln R -> Ln R -> Bool
is_included = flip includes

-- | Apply /f/ to /x/ and /y/ duple of 'Pt'.
pt_fn :: ((a,a) -> b) -> Pt a -> b
pt_fn f p = let (x,y) = pt_xy p in f (x,y)

-- | Apply /f/ to /start/ and /end/ 'Pt' duple of 'Ln'.
ln_fn :: Num a => ((Pt a,Pt a) -> b) -> Ln a -> b
ln_fn f l = let (p,q) = ln_pt l in f (p,q)

-- | Apply /f/ to /start/ and /end/ 'Pt's of 'Ln' and construct 'Ln'.
ln_pt_fn :: (Num a, Num b) => (Pt a -> Pt b) -> Ln a -> Ln b
ln_pt_fn f = ln_fn (\(p,q) -> ln (f p) (f q))

-- | Scale set of 'Ln' to lie in area given by /(0,n)/.
to_unit :: R -> [Ln R] -> [Ln R]
to_unit m p =
    let p' = concatMap (ln_fn (\(i,j) -> [i,j])) p
        x = maximum (map pt_x p')
        y = maximum (map pt_y p')
        f n = pt_fn (\(i,j) -> pt (i*m/n) (m - (j*m/n)))
        g n = ln_pt_fn (f n)
    in map (g (max x y)) p

-- * Orientation

-- | Enumeration of 'Vertical', 'Horizontal' and 'Diagonal'.
data Orientation a = Vertical | Horizontal | Diagonal a
                     deriving (Eq,Show)

-- | Calculate 'Orientation' of 'Ln'.
--
-- > orientation (ln' (0,0) (0,1)) == Vertical
-- > orientation (ln' (0,0) (1,0)) == Horizontal
-- > orientation (ln' (0,0) (1,1)) == Diagonal 1
orientation :: (Fractional a) => Ln a -> Orientation a
orientation l =
    case ln_slope l of
      Nothing -> Vertical
      Just m -> if m == 0 then Horizontal else Diagonal m

-- * Shift Map

-- | A table 'Pt' and 'Orientation' set pairs.
type Shift_Map a = [(Pt a,[Orientation a])]

-- | Construct a 'Shift_Map' from a set of 'Ln's.
mk_shift_map :: [Ln R] -> Shift_Map R
mk_shift_map =
    let f i l = if overlap i l then Just (i,orientation l) else Nothing
        g (x,i,_) = mapMaybe (f i) x
        h (l0,o) = let (p,q) = ln_pt l0 in [(p,o),(q,o)]
    in gather . concatMap h . concatMap g . parts

-- | Apply 'Shift_Map' to a 'Pt'.
shift_map_pt :: Shift_Map R -> Pt R -> Pt R
shift_map_pt tbl i =
    let n = 0.1
        (x,y) = pt_xy i
        g o = let x' = if Vertical `elem` o then x+n else x
                  y' = if Horizontal `elem` o then y+n else y
              in pt x' y'
    in maybe i g (lookup i tbl)

-- | Apply 'Shift_Map' to a 'Ln'.
shift_map_ln :: Shift_Map R -> Ln R -> Ln R
shift_map_ln tbl = ln_pt_fn (shift_map_pt tbl)

-- * Shift table

-- | A table of 'Pt' pairs.
type Shift_Table a = [(Pt a,Pt a)]

-- | Make element of 'Shift_Table'.
mk_shift_tbl_m :: (Ln R,Bool) -> Maybe (Shift_Table R)
mk_shift_tbl_m (l,occ) =
    if occ
    then let (p1,p2) = ln_pt l
             ((x1,y1),(x2,y2)) = ln_pt' l
             n = 0.1
         in if x1 == x2
            then let x = x1 + n in Just [(p1,pt x y1),(p2,pt x y2)]
            else let y = y1 + n in Just [(p1,pt x1 y),(p2,pt x2 y)]
    else Nothing

-- | Make complete 'Shift_Table'.
mk_shift_tbl :: Collision_Table -> Shift_Table R
mk_shift_tbl = concat . mapMaybe mk_shift_tbl_m

-- | Apply 'Shift_Table' to 'Ln'.
shift_table_ln :: Shift_Table R -> Ln R -> Ln R
shift_table_ln tbl =
    let f i = fromMaybe i (lookup i tbl)
    in ln_fn (\(p,q) -> ln (f p) (f q))

-- * Collision table

-- | Table of 'Ln's indicating collisions.
type Collision_Table = [(Ln R,Bool)]

-- | Construct 'Collision_Table' for a set of 'Ln'.
mk_collision_table :: [Ln R] -> Collision_Table
mk_collision_table =
    let f (x,xs) = (x,any (is_included x) xs)
    in map f . parts'

-- | Construct 'Shift_Table' from 'Collision_Table' and shift all 'Ln'.
collision_table_rewrite :: Collision_Table -> [Ln R]
collision_table_rewrite xs =
    let tbl = mk_shift_tbl xs
    in map (shift_table_ln tbl . fst) xs

-- * Path diagram

-- | A diagram given as a set of 'Int' pairs.
type Path_Diagram = [(Int,Int)]

-- | Construct set of 'Ln' from 'Path_Diagram'.
path_diagram_ln :: Path_Diagram -> [Ln R]
path_diagram_ln xs =
    let xs' = map (pt_from_i . pt') xs
    in zipWith ln xs' (tail xs')

-- | 'Collision_Table' based resolution of 'Path_Diagram'.
mk_path_ct :: Path_Diagram -> [Ln R]
mk_path_ct = collision_table_rewrite . mk_collision_table . path_diagram_ln

-- | 'Shift_Map' variant of 'mk_path_ct'.
mk_path_sm :: Path_Diagram -> [Ln R]
mk_path_sm p =
    let p' = path_diagram_ln p
    in map (shift_map_ln (mk_shift_map p')) p'

