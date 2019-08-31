-- | Simple table based spelling rules for common music notation.
module Music.Theory.Pitch.Spelling.Table where

import Data.Maybe {- base -}

import qualified Music.Theory.Pitch as T {- hmt -}
import Music.Theory.Pitch.Note {- hmt -}

type Spelling_Table i = [(i,(Note_T,Alteration_T))]

-- | Spelling table for natural (♮) notes only.
pc_spell_natural_tbl :: Integral i => Spelling_Table i
pc_spell_natural_tbl =
    [(0,(C,Natural))
    ,(2,(D,Natural))
    ,(4,(E,Natural))
    ,(5,(F,Natural))
    ,(7,(G,Natural))
    ,(9,(A,Natural))
    ,(11,(B,Natural))]

-- | Spelling table for sharp (♯) notes only.
pc_spell_sharp_tbl :: Integral i => Spelling_Table i
pc_spell_sharp_tbl =
    [(1,(C,Sharp))
    ,(3,(D,Sharp))
    ,(6,(F,Sharp))
    ,(8,(G,Sharp))
    ,(10,(A,Sharp))]

-- | Spelling table for flat (♭) notes only.
pc_spell_flat_tbl :: Integral i => Spelling_Table i
pc_spell_flat_tbl =
    [(1,(D,Flat))
    ,(3,(E,Flat))
    ,(6,(G,Flat))
    ,(8,(A,Flat))
    ,(10,(B,Flat))]

-- | Spelling table from simplest key-signature.  Note that this is
-- ambiguous for @8@, which could be either G Sharp (♯) in /A Major/
-- or A Flat (♭) in /E Flat (♭) Major/.
pc_spell_ks_tbl :: Integral i => Spelling_Table i
pc_spell_ks_tbl =
      [(1,(C,Sharp)) -- 2♯
      ,(3,(E,Flat)) -- 3♭
      ,(6,(F,Sharp)) -- 1♯
      ,(8,(A,Flat)) -- 3♭/3♯
      ,(10,(B,Flat))] -- 1♭

pc_spell_tbl :: Integral i => Spelling_Table i -> T.Spelling i
pc_spell_tbl tbl = fromMaybe (error "pc_spell_tbl") . flip lookup tbl

-- | Spell using indicated table prepended to and 'pc_spell_natural_tbl' and 'pc_spell_ks_tbl'
pc_spell_tbl_ks :: Integral i => Spelling_Table i -> T.Spelling i
pc_spell_tbl_ks tbl = pc_spell_tbl (tbl ++ pc_spell_natural_tbl ++ pc_spell_ks_tbl)

-- | Spelling for natural (♮) notes only.
--
-- > map pc_spell_natural_m [0,1] == [Just (C,Natural),Nothing]
pc_spell_natural_m :: Integral i => T.Spelling_M i
pc_spell_natural_m = flip lookup pc_spell_natural_tbl

-- | Erroring variant of 'pc_spell_natural_m'.
--
-- > map pc_spell_natural [0,5,7] == [(C,Natural),(F,Natural),(G,Natural)]
pc_spell_natural :: Integral i => T.Spelling i
pc_spell_natural = pc_spell_tbl pc_spell_natural_tbl

-- | Lookup 'pc_spell_ks_tbl'.
--
-- > map pc_spell_ks [6,8] == [(F,Sharp),(A,Flat)]
pc_spell_ks :: Integral i => T.Spelling i
pc_spell_ks = pc_spell_tbl_ks []

-- | Use always sharp (♯) spelling.
--
-- > map pc_spell_sharp [6,8] == [(F,Sharp),(G,Sharp)]
-- > Data.List.nub (map (snd . pc_spell_sharp) [1,3,6,8,10]) == [Sharp]
pc_spell_sharp :: Integral i => T.Spelling i
pc_spell_sharp = pc_spell_tbl (pc_spell_sharp_tbl ++ pc_spell_natural_tbl)

-- | Use always flat (♭) spelling.
--
-- >  map pc_spell_flat [6,8] == [(G,Flat),(A,Flat)]
-- >  Data.List.nub (map (snd . pc_spell_flat) [1,3,6,8,10]) == [Flat]
pc_spell_flat :: Integral i => T.Spelling i
pc_spell_flat = pc_spell_tbl (pc_spell_flat_tbl ++ pc_spell_natural_tbl)

octpc_to_pitch_ks :: Integral i => T.Octave_PitchClass i -> T.Pitch
octpc_to_pitch_ks = T.octpc_to_pitch pc_spell_ks

-- | 'midi_to_pitch' 'T.pc_spell_ks'.
midi_to_pitch_ks :: T.Midi -> T.Pitch
midi_to_pitch_ks = T.midi_to_pitch (pc_spell_ks :: T.Spelling Int)

fmidi_to_pitch_ks :: (Show n,RealFrac n) => n -> T.Pitch
fmidi_to_pitch_ks = T.fmidi_to_pitch_err pc_spell_ks

midi_detune_to_pitch_ks :: Real c => T.Midi_Detune' c -> T.Pitch
midi_detune_to_pitch_ks = T.midi_detune_to_pitch pc_spell_ks
