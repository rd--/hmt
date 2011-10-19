-- | Constants names for ascending 'Interval' values.
module Music.Theory.Interval.Name where

import Music.Theory.Interval

perfect_fourth,perfect_fifth,major_seventh :: Interval
perfect_fourth = Interval Fourth Perfect LT 0
perfect_fifth = Interval Fifth Perfect LT 0
major_seventh = Interval Seventh Major LT 0
