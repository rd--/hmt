-- | Spelling for chromatic clusters.
module Music.Theory.Pitch.Spelling.Cluster where

import Data.List {- base -}

import qualified Music.Theory.Pitch as T
import Music.Theory.Pitch.Name

-- | Spelling table for chromatic and near-chromatic clusters.
--
-- > let f (p,q) = p == sort (map (snd . pitch_to_octpc) q)
-- > in all f spell_cluster_c4_table == True
spell_cluster_c4_table :: [([T.PitchClass],[T.Pitch])]
spell_cluster_c4_table =
    [([0],[c4])
    ,([0,1],[c4,des4])
    ,([0,1,2],[bis3,cis4,d4])
    ,([0,1,2,3],[bis3,cis4,d4,ees4])
    ,([0,1,2,3,10,11],[ais3,b3,c4,cis4,d4,ees4]) -- overlap...
    ,([0,1,2,10],[ais3,bis3,cis4,d4])
    ,([0,1,2,11],[aisis3,bis3,cis4,d4])
    ,([0,1,3],[c4,des4,ees4])
    ,([0,1,3,10],[bes3,c4,des4,ees4])
    ,([0,1,3,11],[b3,c4,des4,ees4])
    ,([0,1,10],[bes3,c4,des4])
    ,([0,1,10,11],[ais3,b3,c4,des4])
    ,([0,1,11],[b3,c4,des4])
    ,([0,2],[c4,d4])
    ,([0,2,3],[c4,d4,ees4])
    ,([0,2,3,10],[bes3,c4,d4,ees4])
    ,([0,2,3,11],[b3,c4,d4,ees4])
    ,([0,2,11],[b3,c4,d4])
    ,([0,2,10],[bes3,c4,d4])
    ,([0,2,10,11],[ais3,b3,c4,d4])
    ,([0,3,10,11],[ais3,b3,c4,dis4])
    ,([0,3,11],[b3,c4,dis4])
    ,([0,10,11],[ais3,b3,c4])
    ,([0,11],[b3,c4])
    ,([1],[cis4])
    ,([1,2],[cis4,d4])
    ,([1,2,3],[cis4,d4,ees4])
    ,([1,2,3,10],[bes3,cis4,d4,ees4])
    ,([1,2,3,11],[b3,cis4,d4,ees4])
    ,([1,2,10],[ais3,cis4,d4])
    ,([1,2,10,11],[ais3,b3,cis4,d4])
    ,([1,2,11],[b3,cis4,d4])
    ,([1,3,11],[b3,cis4,dis4])
    ,([1,3,10,11],[ais3,b3,cis4,dis4])
    ,([1,10,11],[ais3,b3,cis4])
    ,([1,11],[b3,cis4])
    ,([2],[d4])
    ,([2,3],[d4,ees4])
    ,([2,3,4],[d4,ees4,fes4])
    ,([2,3,5],[d4,ees4,f4])
    ,([2,3,4,5],[d4,ees4,fes4,geses4])
    ,([2,3,10,11],[bes3,ces4,d4,ees4])
    ,([2,3,11],[b3,d4,ees4])
    ,([2,4],[d4,e4])
    ,([2,4,5],[d4,e4,f4])
    ,([2,5],[d4,f4])
    ,([2,10,11],[ais3,b3,d4])
    ,([2,11],[b3,d4])
    ,([3],[ees4])
    ,([3,4],[dis4,e4])
    ,([3,4,5],[dis4,e4,f4])
    ,([3,5],[ees4,f4])
    ,([4],[e4])
    ,([4,5],[e4,f4])
    ,([5],[f4])
    ,([5,6],[f4,ges4])
    ,([5,6,7],[eis4,fis4,g4])
    ,([5,6,8],[f4,ges4,aes4])
    ,([5,6,9],[f4,ges4,a4])
    ,([5,6,7,8],[eis4,fis4,g4,aes4])
    ,([5,6,7,8,9],[eis4,fis4,g4,aes4,beses4])
    ,([5,6,7,9],[eis4,fis4,g4,a4])
    ,([5,6,8,9],[eis4,fis4,gis4,a4])
    ,([5,7],[f4,g4])
    ,([5,7,8],[f4,g4,aes4])
    ,([5,7,8,9],[f4,g4,aes4,beses4])
    ,([5,7,9],[f4,g4,a4])
    ,([5,8],[f4,aes4])
    ,([5,8,9],[f4,gis4,a4])
    ,([5,9],[f4,a4])
    ,([6],[fis4])
    ,([6,7],[fis4,g4])
    ,([6,7,8],[fis4,g4,aes4])
    ,([6,7,8,9],[fis4,g4,aes4,beses4])
    ,([6,7,9],[fis4,g4,a4])
    ,([6,8],[fis4,gis4])
    ,([6,8,9],[fis4,gis4,a4])
    ,([6,9],[fis4,a4])
    ,([7],[g4])
    ,([7,8],[g4,aes4])
    ,([7,8,9],[fisis4,gis4,a4])
    ,([7,9],[g4,a4])
    ,([8],[aes4])
    ,([8,9],[gis4,a4])
    ,([8,9,10],[gis4,a4,bes4])
    ,([8,10],[aes4,bes4])
    ,([9],[a4])
    ,([9,10],[a4,bes4])
    ,([10],[bes4])
    ,([10,11],[ais4,b4])
    ,([11],[b4])]

-- | Spelling for chromatic clusters.  Sequence must be ascending.
-- Pitch class @0@ maps to 'c4', if there is no @0@ then all notes are
-- in octave @4@.
--
-- > let f = fmap (map pitch_pp) . spell_cluster_c4
-- > in map f [[11,0],[11]] == [Just ["B3","C4"],Just ["B4"]]
--
-- > fmap (map pitch_pp) (spell_cluster_c4 [10,11]) == Just ["A♯4","B4"]
spell_cluster_c4 :: [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_c4 p = lookup (sort p) spell_cluster_c4_table

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave'.  An
-- octave of @4@ is the identitiy, @3@ an octave below, @5@ an octave
-- above.
--
-- > fmap (map pitch_pp) (spell_cluster_c 3 [11,0]) == Just ["B2","C3"]
-- > fmap (map pitch_pp) (spell_cluster_c 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_c :: T.Octave -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_c o =
    fmap (map (T.pitch_edit_octave (+ (o - 4)))) .
    spell_cluster_c4

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in the octave given by /f/.
--
-- > import Data.Maybe
--
-- > let {f n = if n >= 11 then 3 else 4
-- >     ;g = map pitch_pp .fromJust . spell_cluster_f f
-- >     ;r = [["B3","C4"],["B3"],["C4"],["A♯4","B4"]]}
-- > in map g [[11,0],[11],[0],[10,11]] == r
spell_cluster_f :: (T.PitchClass -> T.Octave) -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_f o_f p =
    let fn r = case r of
                [] -> []
                l:_ -> let (o,n) = T.pitch_to_octpc l
                           f = (+ (o_f n - o))
                       in (map (T.pitch_edit_octave f) r)
    in fmap fn (spell_cluster_c4 p)

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in octave /o/.
--
-- > fmap (map pitch_pp) (spell_cluster_left 3 [11,0]) == Just ["B3","C4"]
-- > fmap (map pitch_pp) (spell_cluster_left 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_left :: T.Octave -> [T.PitchClass] -> Maybe [T.Pitch]
spell_cluster_left o p =
    let fn r = case r of
                [] -> []
                l:_ -> let f = (+ (o - T.octave l))
                       in map (T.pitch_edit_octave f) r
    in fmap fn (spell_cluster_c4 p)
