module Music.Theory.Pitch.Chord where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Text.Parsec as Parsec {- parsec -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Key as Key {- hmt -}
import qualified Music.Theory.Parse as Parse {- hmt -}
import qualified Music.Theory.Pitch.Note as Pitch.Note {- hmt -}

type Pc = (Pitch.Note.Note,Pitch.Note.Alteration)

pc_pp :: Pc -> [Char]
pc_pp (n,a) = Pitch.Note.note_pp n : Pitch.Note.alteration_iso a

-- | D = dominant, M = major
data Extension = D7 | M7 deriving (Eq,Show)

extension_tbl :: Num n => [(Extension, (String,n))]
extension_tbl = [(D7,("7",10)),(M7,("M7",11))]

extension_dat :: Num n => Extension -> (String,n)
extension_dat = flip List.lookup_err extension_tbl

extension_pp :: Extension -> String
extension_pp = fst . (extension_dat :: Extension -> (String,Int))

extension_to_pc :: Num n => Extension -> n
extension_to_pc = snd . extension_dat

data Chord_Type = Major | Minor
                | Augmented | Diminished
                | Diminished_7 | Half_Diminished
                | Suspended_2 | Suspended_4
                  deriving (Eq,Show)

is_suspended :: Chord_Type -> Bool
is_suspended ty = ty `elem` [Suspended_2,Suspended_4]

-- | Names and pc-sets for chord types.
-- The name used here is in the first position, alternates follow.
chord_type_tbl :: Num n => [(Chord_Type,([String],[n]))]
chord_type_tbl =
    [(Major,(["","M","maj"],[0,4,7]))
    ,(Minor,(["m","min"],[0,3,7]))
    ,(Augmented,(["+","aug"],[0,4,8]))
    ,(Diminished,(["o","dim"],[0,3,6]))
    ,(Diminished_7,(["o7","dim7"],[0,3,6,9]))
    ,(Half_Diminished,(["Ø","halfdim","m7(b5)"],[0,3,6,10]))
    ,(Suspended_2,(["sus2"],[0,2,7]))
    ,(Suspended_4,(["sus4"],[0,5,7]))]

chord_type_dat :: Num n => Chord_Type -> ([String],[n])
chord_type_dat = flip List.lookup_err chord_type_tbl

chord_type_pp :: Chord_Type -> String
chord_type_pp = head . fst . (chord_type_dat :: Chord_Type -> ([String],[Int]))

chord_type_pcset :: Num n => Chord_Type -> [n]
chord_type_pcset = snd . chord_type_dat

-- (root,mode,extensions,bass)
data Chord = Chord Pc Chord_Type (Maybe Extension) (Maybe Pc)
             deriving (Show)

chord_pcset :: Chord -> (Maybe Int,[Int])
chord_pcset (Chord pc ty ex bs) =
    let get = m_error "chord_pcset" . Pitch.Note.note_alteration_to_pc
        pc' = get pc
        ty' = chord_type_pcset ty
        ex' = fmap extension_to_pc ex
        bs' = fmap get bs
        ch = map ((`mod` 12) . (+ pc')) (ty' ++ maybe [] return ex')
        ch' = maybe ch (`delete` ch) bs'
    in (bs',ch')

bass_pp :: Pc -> String
bass_pp = ('/' :) . pc_pp

chord_pp :: Chord -> String
chord_pp (Chord pc ty ex bs) =
    let (pre_ty,post_ty) = if is_suspended ty
                           then (Nothing,Just ty)
                           else (Just ty,Nothing)
    in concat [pc_pp pc
              ,maybe "" chord_type_pp pre_ty
              ,maybe "" extension_pp ex
              ,maybe "" chord_type_pp post_ty
              ,maybe "" bass_pp bs]

m_error :: String -> Maybe a -> a
m_error txt = fromMaybe (error txt)

p_pc :: Parse.P Pc
p_pc = do
  n <- Pitch.Note.p_note_t
  a <- Parsec.optionMaybe (Pitch.Note.p_alteration_t_iso True)
  return (n,fromMaybe Pitch.Note.Natural a)

p_mode_m :: Parse.P Key.Mode
p_mode_m = Parsec.option Key.Major_Mode (Parsec.char 'm' >> return Key.Minor_Mode)

p_chord_type :: Parse.P Chord_Type
p_chord_type =
    let m = Parsec.char 'm' >> return Minor
        au = Parsec.char '+' >> return Augmented
        dm = Parsec.char 'o' >> return Diminished
        dm7 = Parsec.try (Parsec.string "o7" >> return Diminished_7)
        hdm = Parsec.char 'Ø' >> return Half_Diminished
        sus2 = Parsec.try (Parsec.string "sus2" >> return Suspended_2)
        sus4 = Parsec.try (Parsec.string "sus4" >> return Suspended_4)
    in Parsec.option Major (Parsec.choice [dm7,dm,hdm,au,sus2,sus4,m])

p_extension :: Parse.P Extension
p_extension =
    let d7 = Parsec.char '7' >> return D7
        m7 = Parsec.try (Parsec.string "M7" >> return M7)
    in Parsec.choice [d7,m7]

p_bass :: Parse.P (Maybe Pc)
p_bass = Parsec.optionMaybe (Parsec.char '/' >> p_pc)

p_chord :: Parse.P Chord
p_chord = do
  pc <- p_pc
  ty <- p_chord_type
  ex <- Parsec.optionMaybe p_extension
  b <- p_bass
  ty' <- p_chord_type
  let ty'' = case (ty,ty') of
               (Major,Suspended_2) -> Suspended_2
               (Major,Suspended_4) -> Suspended_4
               (_,Major) -> ty -- ie. nothing
               _ -> error ("trailing type not sus2 or sus4: " ++ show ty')
  return (Chord pc ty'' ex b)

{- | Parse chord.

>>> let txt = words "CmM7 C#o EbM7 Fo7 Gx/D C/E GØ/F Bbsus4/C E7sus2"
>>> let chd = map parse_chord txt
>>> map chord_pp chd == txt
True

>>> map (snd . chord_pcset) chd
[[0,3,7,11],[1,4,7],[3,7,10,2],[5,8,11,2],[9,1,4],[0,7],[7,10,1],[10,3,5],[4,6,11,2]]

>>> map (fst . chord_pcset) chd
[Nothing,Nothing,Nothing,Nothing,Just 2,Just 4,Just 5,Just 0,Nothing]
-}
parse_chord :: String -> Chord
parse_chord =
    either (\e -> error ("parse_chord failed\n" ++ show e)) id .
    Parsec.parse p_chord ""
