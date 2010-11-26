module Music.Theory.Spelling where

import Music.Theory.Interval
import Music.Theory.Pitch

pc_spell_natural :: PitchClass -> (Note_T, Alteration_T)
pc_spell_natural pc =
    case pc of
      0 -> (C,Natural)
      2 -> (D,Natural)
      4 -> (E,Natural)
      5 -> (F,Natural)
      7 -> (G,Natural)
      9 -> (A,Natural)
      11 -> (B,Natural)
      _ -> error ("pc_spell_natural: " ++ show pc)

-- use spelling from simplest key-signature
-- ambiguous for 8 (G#/Ab)
pc_spell_ks :: PitchClass -> (Note_T, Alteration_T)
pc_spell_ks pc =
    case pc of
      1 -> (C,Sharp) -- 2#
      3 -> (E,Flat) -- 3b
      6 -> (F,Sharp) -- 1#
      8 -> (A,Flat) -- 3b/3#
      10 -> (B,Flat) -- 1b
      _ -> pc_spell_natural pc

pc_spell_sharp :: PitchClass -> (Note_T, Alteration_T)
pc_spell_sharp pc =
    case pc of
      1 -> (C,Sharp)
      3 -> (D,Sharp)
      6 -> (F,Sharp)
      8 -> (G,Sharp)
      10 -> (A,Sharp)
      _ -> pc_spell_natural pc

pc_spell_flat :: PitchClass -> (Note_T, Alteration_T)
pc_spell_flat pc =
    case pc of
      1 -> (D,Sharp)
      3 -> (E,Flat)
      6 -> (G,Flat)
      8 -> (A,Flat)
      10 -> (B,Flat)
      _ -> pc_spell_natural pc

-- ambiguous for 6 (aug.4,dim.5)
i_to_interval :: Int -> Interval
i_to_interval x =
    let iv ty qu = Interval ty qu LT 0
    in case x of
         0 -> iv Unison Perfect
         1 -> iv Second Minor
         2 -> iv Second Major
         3 -> iv Third Minor
         4 -> iv Third Major
         5 -> iv Fourth Perfect
         6 -> iv Fourth Augmented -- Fifth Diminished
         7 -> iv Fifth Perfect
         8 -> iv Sixth Minor
         9 -> iv Sixth Major
         10 -> iv Seventh Minor
         11 -> iv Seventh Major
         _ -> error ("i_to_interval: " ++ show x)

-- for non-tonal music some spellings are poor, ie. (f,g#)
interval_simplify :: Interval -> Interval
interval_simplify x =
    let (Interval ty qu d o) = x
        (qu',ty') = case (qu,ty) of
                     (Diminished,Second) -> (Perfect,Unison)
                     (Diminished,Third) -> (Major,Second)
                     (Augmented,Second) -> (Minor,Third)
                     (Augmented,Third) -> (Perfect,Fourth)
                     (Diminished,Sixth) -> (Perfect,Fifth)
                     (Diminished,Seventh) -> (Major,Sixth)
                     (Augmented,Sixth) -> (Minor,Seventh)
                     -- (Augmented,Seventh) -> (Perfect,Octave)
                     _ -> (qu,ty)
    in Interval ty' qu' d o

{-
map pc_spell_ks [0..11]
map i_to_interval [0..11]
-}
