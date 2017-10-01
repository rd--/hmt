-- | Spelling rules for common music notation.
module Music.Theory.Pitch.Spelling where

import Data.Maybe {- base -}

import qualified Music.Theory.List as T {- hmt -}
import Music.Theory.Pitch.Note (Note_T(..),Alteration_T(..)) {- hmt -}

-- | Function to spell a 'PitchClass'.
type Spelling n = n -> (Note_T,Alteration_T)

-- | Variant of 'Spelling' for incomplete functions.
type Spelling_M i = i -> Maybe (Note_T, Alteration_T)

-- | Spelling table for natural (♮) notes only.
pc_spell_natural_tbl :: Integral i => [(i,(Note_T,Alteration_T))]
pc_spell_natural_tbl =
    [(0,(C,Natural))
    ,(2,(D,Natural))
    ,(4,(E,Natural))
    ,(5,(F,Natural))
    ,(7,(G,Natural))
    ,(9,(A,Natural))
    ,(11,(B,Natural))]

-- | Spelling table for sharp (♯) notes only.
pc_spell_sharp_tbl :: Integral i => [(i,(Note_T,Alteration_T))]
pc_spell_sharp_tbl =
    [(1,(C,Sharp))
    ,(3,(D,Sharp))
    ,(6,(F,Sharp))
    ,(8,(G,Sharp))
    ,(10,(A,Sharp))]

-- | Spelling table for flat (♭) notes only.
pc_spell_flat_tbl :: Integral i => [(i,(Note_T,Alteration_T))]
pc_spell_flat_tbl =
    [(1,(D,Flat))
    ,(3,(E,Flat))
    ,(6,(G,Flat))
    ,(8,(A,Flat))
    ,(10,(B,Flat))]

-- | Spelling table from simplest key-signature.  Note that this is
-- ambiguous for @8@, which could be either G Sharp (♯) in /A Major/
-- or A Flat (♭) in /E Flat (♭) Major/.
pc_spell_ks_tbl :: Integral i => [(i,(Note_T,Alteration_T))]
pc_spell_ks_tbl =
      [(1,(C,Sharp)) -- 2♯
      ,(3,(E,Flat)) -- 3♭
      ,(6,(F,Sharp)) -- 1♯
      ,(8,(A,Flat)) -- 3♭/3♯
      ,(10,(B,Flat))] -- 1♭

-- | Spelling for natural (♮) notes only.
--
-- > map pc_spell_natural_m [0,1] == [Just (C,Natural),Nothing]
pc_spell_natural_m :: Integral i => Spelling_M i
pc_spell_natural_m pc = lookup pc pc_spell_natural_tbl

-- | Erroring variant of 'pc_spell_natural_m'.
--
-- > map pc_spell_natural [0,5,7] == [(C,Natural),(F,Natural),(G,Natural)]
pc_spell_natural :: Integral i => Spelling i
pc_spell_natural = fromMaybe (error "pc_spell_natural") . pc_spell_natural_m

-- | Use spelling from simplest key-signature.  Note that this is
-- ambiguous for @8@, which could be either G Sharp (♯) in /A Major/
-- or A Flat (♭) in /E Flat (♭) Major/.
--
-- > map pc_spell_ks [6,8] == [(F,Sharp),(A,Flat)]
pc_spell_ks :: Integral i => Spelling i
pc_spell_ks pc = T.lookup_err pc (pc_spell_natural_tbl ++ pc_spell_ks_tbl)

-- | Use always sharp (♯) spelling.
--
-- > map pc_spell_sharp [6,8] == [(F,Sharp),(G,Sharp)]
-- > Data.List.nub (map (snd . pc_spell_sharp) [1,3,6,8,10]) == [Sharp]
pc_spell_sharp :: Integral i => Spelling i
pc_spell_sharp pc = T.lookup_err pc (pc_spell_natural_tbl ++ pc_spell_sharp_tbl)

-- | Use always flat (♭) spelling.
--
-- >  map pc_spell_flat [6,8] == [(G,Flat),(A,Flat)]
-- >  Data.List.nub (map (snd . pc_spell_flat) [1,3,6,8,10]) == [Flat]
pc_spell_flat :: Integral i => Spelling i
pc_spell_flat pc = T.lookup_err pc (pc_spell_natural_tbl ++ pc_spell_flat_tbl)
