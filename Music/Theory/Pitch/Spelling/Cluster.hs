-- | Spelling for chromatic clusters.
module Music.Theory.Pitch.Spelling.Cluster where

import Data.List
import Music.Theory.Pitch
import Music.Theory.Pitch.Name

-- | Spelling for chromatic clusters.  Sequence must be ascending.
-- Pitch class @0@ maps to 'c4', of there is no @0@ the all notes are
-- in octave @4@.
--
-- > let f = map pitch_pp . spell_cluster_c4
-- > in map f [[11,0],[11]] == [["B3","C4"],["B4"]]
--
-- > map pitch_pp (spell_cluster_c4 [10,11]) == ["B♭4","B4"]
spell_cluster_c4 :: [PitchClass] -> [Pitch]
spell_cluster_c4 p =
    case sort p of
      [0] -> [c4]
      [0,1] -> [c4,des4]
      [0,1,2] -> [bis3,cis4,d4]
      [0,1,2,11] -> [aisis3,bis3,cis4,d4]
      [0,1,11] -> [b3,c4,des4]
      [0,2] -> [c4,d4]
      [0,2,11] -> [b3,c4,d4]
      [0,11] -> [b3,c4]
      [1] -> [cis4]
      [1,2] -> [cis4,d4]
      [1,2,11] -> [b3,cis4,d4]
      [1,11] -> [b3,cis4]
      [2] -> [d4]
      [2,3] -> [d4,ees4]
      [2,3,4] -> [d4,ees4,fes4]
      [2,3,5] -> [d4,ees4,f4]
      [2,3,4,5] -> [d4,ees4,fes4,geses4]
      [2,4] -> [d4,e4]
      [2,4,5] -> [d4,e4,f4]
      [2,5] -> [d4,f4]
      [2,11] -> [b3,d4]
      [3] -> [ees4]
      [3,4] -> [dis4,e4]
      [3,4,5] -> [dis4,e4,f4]
      [3,5] -> [ees4,f4]
      [4] -> [e4]
      [4,5] -> [e4,f4]
      [5] -> [f4]
      [5,6] -> [f4,ges4]
      [5,6,7] -> [eis4,f4,ges4]
      [5,6,8] -> [f4,ges4,aes4]
      [5,6,7,8] -> [eis4,fis4,g4,aes4]
      [5,7] -> [f4,g4]
      [5,7,8] -> [f4,g4,aes4]
      [5,8] -> [f4,aes4]
      [6] -> [fis4]
      [6,7] -> [fis4,g4]
      [6,7,8] -> [fis4,g4,aes4]
      [6,8] -> [f4,aes4]
      [7] -> [g4]
      [7,8] -> [g4,aes4]
      [8] -> [aes4]
      [10,11] -> [bes4,b4]
      [11] -> [b4]
      _ -> error ("spell_cluster: " ++ show p)

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave'.  An
-- octave of @4@ is the identitiy, @3@ an octave below, @5@ an octave
-- above.
--
-- > map pitch_pp (spell_cluster_c 3 [11,0]) == ["B2","C3"]
-- > map pitch_pp (spell_cluster_c 3 [10,11]) == ["B♭3","B3"]
spell_cluster_c :: Octave -> [PitchClass] -> [Pitch]
spell_cluster_c o = map (pitch_edit_octave (+ (o - 4))) . spell_cluster_c4

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in the octave given by /f/.
--
-- > let {f n = if n >= 11 then 3 else 4
-- >     ;g = map pitch_pp . spell_cluster_f f
-- >     ;r = [["B3","C4"],["B3"],["C4"],["B♭4","B4"]]}
-- > in map g [[11,0],[11],[0],[10,11]] == r
spell_cluster_f :: (PitchClass -> Octave) -> [PitchClass] -> [Pitch]
spell_cluster_f o_f p =
    let r = spell_cluster_c4 p
    in case r of
         [] -> []
         l:_ -> let (o,n) = pitch_to_octpc l
                    f = (+ (o_f n - o))
                in map (pitch_edit_octave f) r

-- | Variant of 'spell_cluster_c4' that runs 'pitch_edit_octave' so
-- that the left-most note is in octave /o/.
--
-- > map pitch_pp (spell_cluster_left 3 [11,0]) == ["B3","C4"]
-- > map pitch_pp (spell_cluster_left 3 [10,11]) == ["B♭3","B3"]
spell_cluster_left :: Octave -> [PitchClass] -> [Pitch]
spell_cluster_left o p =
    let r = spell_cluster_c4 p
    in case r of
         [] -> []
         l:_ -> let f = (+ (o - octave l))
                in map (pitch_edit_octave f) r
