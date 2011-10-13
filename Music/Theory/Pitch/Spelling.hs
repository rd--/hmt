-- | Spelling rules for common music notation.
module Music.Theory.Pitch.Spelling where

import Music.Theory.Pitch

-- | Variant of 'Spelling' for incomplete functions.
type Spelling_M = PitchClass -> Maybe (Note_T, Alteration_T)

-- | Spelling for natural (♮) notes only.
--
-- > map pc_spell_natural_m [0,1] == [Just (C,Natural),Nothing]
pc_spell_natural_m :: Spelling_M
pc_spell_natural_m pc =
    case pc of
      0 -> Just (C,Natural)
      2 -> Just (D,Natural)
      4 -> Just (E,Natural)
      5 -> Just (F,Natural)
      7 -> Just (G,Natural)
      9 -> Just (A,Natural)
      11 -> Just (B,Natural)
      _ -> Nothing

-- | Erroring variant of 'pc_spell_natural_m'.
--
-- > map pc_spell_natural [0,5,7] == [(C,Natural),(F,Natural),(G,Natural)]
pc_spell_natural :: Spelling
pc_spell_natural pc =
    case pc_spell_natural_m pc of
      Just p -> p
      _ -> error ("pc_spell_natural: " ++ show pc)

-- | Use spelling from simplest key-signature.  Note that this is
-- ambiguous for @8@, which could be either G Sharp (♯) in /A Major/
-- or A Flat (♭) in /E Flat (♭) Major/.
--
-- > map pc_spell_ks [6,8] == [(F,Sharp),(A,Flat)]
pc_spell_ks :: Spelling
pc_spell_ks pc =
    case pc of
      1 -> (C,Sharp) -- 2#
      3 -> (E,Flat) -- 3b
      6 -> (F,Sharp) -- 1#
      8 -> (A,Flat) -- 3b/3#
      10 -> (B,Flat) -- 1b
      _ -> pc_spell_natural pc

-- | Use always sharp (♯) spelling.
--
-- > map pc_spell_sharp [6,8] == [(F,Sharp),(G,Sharp)]
-- > Data.List.nub (map (snd . pc_spell_sharp) [1,3,6,8,10]) == [Sharp]
-- > octpc_to_pitch pc_spell_sharp (4,6) == Pitch F Sharp 4
pc_spell_sharp :: Spelling
pc_spell_sharp pc =
    case pc of
      1 -> (C,Sharp)
      3 -> (D,Sharp)
      6 -> (F,Sharp)
      8 -> (G,Sharp)
      10 -> (A,Sharp)
      _ -> pc_spell_natural pc

-- | Use always flat (♭) spelling.
--
-- >  map pc_spell_flat [6,8] == [(G,Flat),(A,Flat)]
-- >  Data.List.nub (map (snd . pc_spell_flat) [1,3,6,8,10]) == [Flat]
pc_spell_flat :: Spelling
pc_spell_flat pc =
    case pc of
      1 -> (D,Flat)
      3 -> (E,Flat)
      6 -> (G,Flat)
      8 -> (A,Flat)
      10 -> (B,Flat)
      _ -> pc_spell_natural pc
