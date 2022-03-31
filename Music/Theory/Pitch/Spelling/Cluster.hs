-- | Spelling for chromatic clusters.
module Music.Theory.Pitch.Spelling.Cluster where

import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}
import           Music.Theory.Pitch.Note.Name {- hmt -}

-- | Form of cluster with smallest outer boundary interval.
--
-- > cluster_normal_order [0,1,11] == [11,0,1]
cluster_normal_order :: [T.PitchClass] -> [T.PitchClass]
cluster_normal_order =
    let with_bounds x = ((last x - head x) `mod` 12,x)
    in snd . minimum . map with_bounds . T.rotations

-- | Normal order starting in indicated octave.
--
-- > cluster_normal_order_octpc 3 [0,1,11] == [(3,11),(4,0),(4,1)]
cluster_normal_order_octpc :: T.Octave -> [T.PitchClass] -> [T.OctPc]
cluster_normal_order_octpc o pc =
    let pc_n = cluster_normal_order pc
        pc_0 = head pc_n
    in map (\x -> (if x >= pc_0 then o else o + 1,x)) pc_n

-- | True if 'sort' of cluster is not equal to 'cluster_normal_order'.
--
-- > map cluster_is_multiple_octave [[0,1,11],[1,2,3],[1,2,11]] == [True,False,True]
cluster_is_multiple_octave :: [T.PitchClass] -> Bool
cluster_is_multiple_octave x = sort x /= cluster_normal_order x

-- | Spelling table for chromatic and near-chromatic clusters,
-- pitch-classes are in cluster order.
--
-- > let f (p,q) = (p == map T.note_alteration_to_pc_err q)
-- > in all f spell_cluster_table
spell_cluster_table :: [([T.PitchClass],[(T.Note_T,T.Alteration_T)])]
spell_cluster_table =
    [([0,1,2,3],[bis,cis,d,ees])
    ,([0,1,2],[bis,cis,d])
    ,([0,1,3],[c,des,ees])
    ,([0,1],[c,des])
    ,([0,2,3],[c,d,ees])
    ,([0,2],[c,d])
    ,([0],[c])
    ,([1,2,3],[cis,d,ees])
    ,([1,2],[cis,d])
    ,([10,0,1,2],[ais,bis,cis,d])
    ,([10,0,1,3],[bes,c,des,ees])
    ,([10,0,1],[bes,c,des])
    ,([10,0,2,3],[bes,c,d,ees])
    ,([10,0,2],[bes,c,d])
    ,([10,1,2,3],[bes,cis,d,ees])
    ,([10,1,2],[ais,cis,d])
    ,([10,11,0,1,2,3],[ais,b,c,cis,d,ees]) -- overlap...
    ,([10,11,0,1],[ais,b,c,des])
    ,([10,11,0,2],[ais,b,c,d])
    ,([10,11,0,3],[ais,b,c,dis])
    ,([10,11,0],[ais,b,c])
    ,([10,11,1,2],[ais,b,cis,d])
    ,([10,11,1,3],[ais,b,cis,dis])
    ,([10,11,1],[ais,b,cis])
    ,([10,11,2,3],[bes,ces,d,ees])
    ,([10,11,2],[ais,b,d])
    ,([10,11],[ais,b])
    ,([10],[bes])
    ,([11,0,1,2],[aisis,bis,cis,d])
    ,([11,0,1,3],[b,c,des,ees])
    ,([11,0,1],[b,c,des])
    ,([11,0,2,3],[b,c,d,ees])
    ,([11,0,2],[b,c,d])
    ,([11,0,3],[b,c,dis])
    ,([11,0],[b,c])
    ,([11,1,2,3],[b,cis,d,ees])
    ,([11,1,2],[b,cis,d])
    ,([11,1,3],[b,cis,dis])
    ,([11,1],[b,cis])
    ,([11,2,3],[b,d,ees])
    ,([11,2],[b,d])
    ,([11],[b])
    ,([1],[cis])
    ,([2,3,4,5],[d,ees,fes,geses])
    ,([2,3,4],[d,ees,fes])
    ,([2,3,5],[d,ees,f])
    ,([2,3],[d,ees])
    ,([2,4,5],[d,e,f])
    ,([2,4],[d,e])
    ,([2,5],[d,f])
    ,([2],[d])
    ,([3,4,5],[dis,e,f])
    ,([3,4],[dis,e])
    ,([3,5],[ees,f])
    ,([3],[ees])
    ,([4,5],[e,f])
    ,([4],[e])
    ,([5,6,7,8,9],[eis,fis,g,aes,beses])
    ,([5,6,7,8],[eis,fis,g,aes])
    ,([5,6,7,9],[eis,fis,g,a])
    ,([5,6,7],[eis,fis,g])
    ,([5,6,8,9],[eis,fis,gis,a])
    ,([5,6,8],[f,ges,aes])
    ,([5,6,9],[f,ges,a])
    ,([5,6],[f,ges])
    ,([5,7,8,9],[f,g,aes,beses])
    ,([5,7,8],[f,g,aes])
    ,([5,7,9],[f,g,a])
    ,([5,7],[f,g])
    ,([5,8,9],[f,gis,a])
    ,([5,8],[f,aes])
    ,([5,9],[f,a])
    ,([5],[f])
    ,([6,7,8,9],[fis,g,aes,beses])
    ,([6,7,8],[fis,g,aes])
    ,([6,7,9],[fis,g,a])
    ,([6,7],[fis,g])
    ,([6,8,9],[fis,gis,a])
    ,([6,8],[fis,gis])
    ,([6,9],[fis,a])
    ,([6],[fis])
    ,([7,8,9],[fisis,gis,a])
    ,([7,8],[g,aes])
    ,([7,9],[g,a])
    ,([7],[g])
    ,([8,10],[aes,bes])
    ,([8,9,10],[gis,a,bes])
    ,([8,9],[gis,a])
    ,([8],[aes])
    ,([9,10],[a,bes])
    ,([9],[a])]

spell_cluster :: [T.PitchClass] -> Maybe [(T.Note_T,T.Alteration_T)]
spell_cluster = flip lookup spell_cluster_table

-- | Spell an arbitrary sequence of 'T.OctPc' values.
--
-- > fmap (map T.pitch_pp_iso) (spell_cluster_octpc [(3,11),(4,3),(4,11),(5,1)])
spell_cluster_octpc :: [T.OctPc] -> Maybe [T.Pitch]
spell_cluster_octpc o =
    let p = cluster_normal_order (sort (nub (map snd o)))
        na_f na =
            let na_tbl = map (\x -> (T.note_alteration_to_pc_err x,x)) na
                o_f (oct,pc) = let (n,alt) = T.lookup_err pc na_tbl in T.Pitch n alt oct
            in map o_f o
    in fmap na_f (spell_cluster p)

-- | Spelling for chromatic clusters.  Sequence must be ascending.
-- Pitch class @0@ maps to 'c4', if there is no @0@ then all notes are
-- in octave @4@.
--
-- > let f = (fmap (map T.pitch_pp) . spell_cluster_c4)
-- > map f [[11,0],[11],[0,11]] == [Just ["B3","C4"],Just ["B4"],Nothing]
--
-- > fmap (map T.pitch_pp) (spell_cluster_c4 [10,11]) == Just ["A♯4","B4"]
spell_cluster_c4 :: [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_c4 p =
    let o_0 = if cluster_is_multiple_octave p then 3 else 4
        oct = map fst (cluster_normal_order_octpc o_0 p)
    in case spell_cluster p of
         Nothing -> Nothing
         Just na -> Just (zipWith (\(n,alt) o -> T.Pitch n alt o) na oct)

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave'.  An
-- octave of @4@ is the identitiy, @3@ an octave below, @5@ an octave
-- above.
--
-- > fmap (map T.pitch_pp) (spell_cluster_c 3 [11,0]) == Just ["B2","C3"]
-- > fmap (map T.pitch_pp) (spell_cluster_c 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_c :: T.Octave -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_c o =
    fmap (map (T.pitch_edit_octave (+ (o - 4)))) .
    spell_cluster_c4

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in the octave given by /f/.
--
-- > import Data.Maybe
--
-- > let f n = if n >= 11 then 3 else 4
-- > let g = map T.pitch_pp .fromJust . spell_cluster_f f
-- > let r = [["B3","C4"],["B3"],["C4"],["A♯4","B4"]]
-- > map g [[11,0],[11],[0],[10,11]] == r
--
-- > map (spell_cluster_f (const 4)) [[0,11],[11,0],[6,7],[7,6]]
spell_cluster_f :: (T.PitchClass -> T.Octave) -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_f o_f p =
    let fn r = case r of
                [] -> []
                l:_ -> let (o,n) = T.pitch_to_octpc l
                           oct_f = (+ (o_f n - o))
                       in map (T.pitch_edit_octave oct_f) r
    in fmap fn (spell_cluster_c4 p)

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in octave /o/.
--
-- > fmap (map T.pitch_pp) (spell_cluster_left 3 [11,0]) == Just ["B3","C4"]
-- > fmap (map T.pitch_pp) (spell_cluster_left 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_left :: T.Octave -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_left o p =
    let fn r = case r of
                [] -> []
                l:_ -> let oct_f = (+ (o - T.octave l))
                       in map (T.pitch_edit_octave oct_f) r
    in fmap fn (spell_cluster_c4 p)
