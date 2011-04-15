module Music.Theory.Interval.Spelling where

import Music.Theory.Interval

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
map i_to_interval [0..11]
-}
