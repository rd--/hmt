-- | Spelling for chromatic clusters.
module Music.Theory.Pitch.Spelling.Cluster where

import Data.List
import Music.Theory.Pitch
import Music.Theory.Pitch.Name

-- | Spelling for chromatic clusters.  Sequence must be ascending.
-- Pitch class @0@ maps to 'c4', of there is no @0@ the all notes are
-- in octave @4@.
--
-- > let f = fmap (map pitch_pp) . spell_cluster_c4
-- > in map f [[11,0],[11]] == [Just ["B3","C4"],Just ["B4"]]
--
-- > fmap (map pitch_pp) (spell_cluster_c4 [10,11]) == Just ["A♯4","B4"]
spell_cluster_c4 :: [PitchClass] -> Maybe [Pitch]
spell_cluster_c4 p =
    case sort p of
      [0] -> Just [c4]
      [0,1] -> Just [c4,des4]
      [0,1,2] -> Just [bis3,cis4,d4]
      [0,1,2,11] -> Just [aisis3,bis3,cis4,d4]
      [0,1,11] -> Just [b3,c4,des4]
      [0,2] -> Just [c4,d4]
      [0,2,11] -> Just [b3,c4,d4]
      [0,11] -> Just [b3,c4]
      [1] -> Just [cis4]
      [1,2] -> Just [cis4,d4]
      [1,2,11] -> Just [b3,cis4,d4]
      [1,11] -> Just [b3,cis4]
      [2] -> Just [d4]
      [2,3] -> Just [d4,ees4]
      [2,3,4] -> Just [d4,ees4,fes4]
      [2,3,5] -> Just [d4,ees4,f4]
      [2,3,4,5] -> Just [d4,ees4,fes4,geses4]
      [2,4] -> Just [d4,e4]
      [2,4,5] -> Just [d4,e4,f4]
      [2,5] -> Just [d4,f4]
      [2,11] -> Just [b3,d4]
      [3] -> Just [ees4]
      [3,4] -> Just [dis4,e4]
      [3,4,5] -> Just [dis4,e4,f4]
      [3,5] -> Just [ees4,f4]
      [4] -> Just [e4]
      [4,5] -> Just [e4,f4]
      [5] -> Just [f4]
      [5,6] -> Just [f4,ges4]
      [5,6,7] -> Just [eis4,fis4,g4]
      [5,6,8] -> Just [f4,ges4,aes4]
      [5,6,9] -> Just [f4,ges4,a4]
      [5,6,7,8] -> Just [eis4,fis4,g4,aes4]
      [5,7] -> Just [f4,g4]
      [5,7,8] -> Just [f4,g4,aes4]
      [5,7,9] -> Just [f4,g4,a4]
      [5,8] -> Just [f4,aes4]
      [5,8,9] -> Just [f4,gis4,a4]
      [5,9] -> Just [f4,a4]
      [6] -> Just [fis4]
      [6,7] -> Just [fis4,g4]
      [6,7,8] -> Just [fis4,g4,aes4]
      [6,7,9] -> Just [fis4,g4,a4]
      [6,8] -> Just [fis4,gis4]
      [6,8,9] -> Just [fis4,gis4,a4]
      [6,9] -> Just [fis4,a4]
      [7] -> Just [g4]
      [7,8] -> Just [g4,aes4]
      [7,8,9] -> Just [fisis4,gis4,a4]
      [7,9] -> Just [g4,a4]
      [8] -> Just [aes4]
      [8,9] -> Just [gis4,a4]
      [8,9,10] -> Just [gis4,a4,bes4]
      [8,10] -> Just [aes4,bes4]
      [9] -> Just [a4]
      [9,10] -> Just [a4,bes4]
      [10] -> Just [bes4]
      [10,11] -> Just [ais4,b4]
      [11] -> Just [b4]
      _ -> Nothing

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave'.  An
-- octave of @4@ is the identitiy, @3@ an octave below, @5@ an octave
-- above.
--
-- > fmap (map pitch_pp) (spell_cluster_c 3 [11,0]) == Just ["B2","C3"]
-- > fmap (map pitch_pp) (spell_cluster_c 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_c :: Octave -> [PitchClass] -> Maybe [Pitch]
spell_cluster_c o =
    fmap (map (pitch_edit_octave (+ (o - 4)))) .
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
spell_cluster_f :: (PitchClass -> Octave) -> [PitchClass] -> Maybe [Pitch]
spell_cluster_f o_f p =
    let fn r = case r of
                [] -> []
                l:_ -> let (o,n) = pitch_to_octpc l
                           f = (+ (o_f n - o))
                       in (map (pitch_edit_octave f) r)
    in fmap fn (spell_cluster_c4 p)

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in octave /o/.
--
-- > fmap (map pitch_pp) (spell_cluster_left 3 [11,0]) == Just ["B3","C4"]
-- > fmap (map pitch_pp) (spell_cluster_left 3 [10,11]) == Just ["A♯3","B3"]
spell_cluster_left :: Octave -> [PitchClass] -> Maybe [Pitch]
spell_cluster_left o p =
    let fn r = case r of
                [] -> []
                l:_ -> let f = (+ (o - octave l))
                       in map (pitch_edit_octave f) r
    in fmap fn (spell_cluster_c4 p)
