-- | Spelling rules for common music notation.
module Music.Theory.Pitch.Spelling where

import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Cluster as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Key as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as T {- hmt -}

spell_octpc_set :: [T.OctPC] -> [T.Pitch]
spell_octpc_set o =
  case T.octpc_spell_implied_key o of
    Just r -> r
    Nothing ->
      case T.spell_cluster_octpc o of
        Just r -> r
        Nothing -> map T.octpc_to_pitch_ks o

spell_midi_set :: [T.Midi] -> [T.Pitch]
spell_midi_set = spell_octpc_set . map T.midi_to_octpc
