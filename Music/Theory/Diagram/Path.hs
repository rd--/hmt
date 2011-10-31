-- | Functions to make /path/ diagrams such as those in Fig. VIII-11
-- on I.Xenakis /Formalized Music/.
module Music.Theory.Diagram.Path where

import Data.CG.Minus
import Data.CG.Minus.Colour
import Data.Colour
import Data.Function
import Data.List
import Data.Maybe
import qualified Graphics.Rendering.Cairo as C
import Render.CG.Minus.Arrow

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

-- * Shifts

mk_shift_map :: [Ln R] -> [(Pt R,[Orientation R])]
mk_shift_map =
    let f i l = if overlap i l then Just (i,orientation l) else Nothing
        g (x,i,_) = mapMaybe (f i) x
        h (l0,o) = let (p,q) = ln_pt l0 in [(p,o),(q,o)]
    in gather . concatMap h . concatMap g . parts

do_shift :: [(Pt R,[Orientation R])] -> Pt R -> Pt R
do_shift tbl i =
    let n = 0.1
        (x,y) = pt_xy i
        g o = let x' = if Vertical `elem` o then x+n else x
                  y' = if Horizontal `elem` o then y+n else y
              in pt x' y'
    in maybe i g (lookup i tbl)

do_shift' :: [(Pt R,[Orientation R])] -> Ln R -> Ln R
do_shift' tbl = ln_fn (\(p,q) -> ln (do_shift tbl p) (do_shift tbl q))

note_coll :: [Ln R] -> [(Ln R,Bool)]
note_coll =
    let f (x,xs) = (x,any (is_included x) xs)
    in map f . parts'

shift_tbl :: (Ln R,Bool) -> Maybe [(Pt R,Pt R)]
shift_tbl (l,occ) =
    if occ
    then let (p1,p2) = ln_pt l
             ((x1,y1),(x2,y2)) = ln_pt' l
             n = 0.1
         in if x1 == x2
            then let x = x1 + n in Just [(p1,pt x y1),(p2,pt x y2)]
            else let y = y1 + n in Just [(p1,pt x1 y),(p2,pt x2 y)]
    else Nothing

shift_tbl' :: [(Ln R,Bool)] -> [(Pt R,Pt R)]
shift_tbl' = concat . mapMaybe shift_tbl

trans :: [(Pt R,Pt R)] -> Ln R -> Ln R
trans tbl =
    let f i = fromMaybe i (lookup i tbl)
    in ln_fn (\(p,q) -> ln (f p) (f q))

replc :: [(Ln R,Bool)] -> [Ln R]
replc xs =
    let tbl = shift_tbl' xs
    in map (trans tbl . fst) xs

to_pts :: [(Int,Int)] -> [Ln R]
to_pts xs =
    let xs' = map (pt_from_i . pt') xs
    in zipWith ln xs' (tail xs')

mk_path :: [(Int,Int)] -> [Ln R]
mk_path = replc . note_coll . to_pts

mk_path' :: [(Int,Int)] -> [Ln R]
mk_path' p =
    let p' = to_pts p
    in map (do_shift' (mk_shift_map p')) p'

-- | Apply /f/ to /x/ and /y/ duple of 'Pt'.
pt_fn :: ((a,a) -> b) -> Pt a -> b
pt_fn f p = let (x,y) = pt_xy p in f (x,y)

-- | Apply /f/ to /start/ and /end/ 'Pt' duple of 'Ln'.
ln_fn :: Num a => ((Pt a,Pt a) -> b) -> Ln a -> b
ln_fn f l = let (p,q) = ln_pt l in f (p,q)

to_unit :: R -> [Ln R] -> [Ln R]
to_unit m p =
    let p' = concatMap (ln_fn (\(i,j) -> [i,j])) p
        x = maximum (map pt_x p')
        y = maximum (map pt_y p')
        f n = pt_fn (\(i,j) -> pt (i*m/n) (m - (j*m/n)))
        g n = ln_fn (\(i,j) -> ln (f n i) (f n j))
    in map (g (max x y)) p

-- * Path

type Path = [(Ca,Ls R)]

draw_path :: Path -> C.Render ()
draw_path xs = do
  mapM_ (uncurry (arrows_mp 0.1 (pi/9))) xs
  C.showPage

draw_paths :: [Path] -> C.Render ()
draw_paths = mapM_ draw_path

write_pdf :: FilePath -> [Path] -> IO ()
write_pdf fn xs = do
  let f s = C.renderWith s (C.translate 10 100 >>
                            C.scale 100 100 >>
                            draw_paths xs)
  C.withPDFSurface fn 500 500 f

-- * Path diagram

path_diagram :: FilePath -> [[(Int,Int)]] -> IO ()
path_diagram fn =
    let f (i,j) = (opaque black,[i,j])
    in write_pdf fn . map (map (ln_fn f) . to_unit 4 . mk_path')

