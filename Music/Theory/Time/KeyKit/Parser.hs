-- | KeyKit phrase literal (constant) parser and printer.
module Music.Theory.Time.KeyKit.Parser where

import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as String {- parsec -}

-- * Parser setup

-- | A 'Char' parser with no user state.
type P a = String.GenParser Char () a

-- | Run parser and return either an error string or an answer.
kk_parse_either :: P t -> String -> Either String t
kk_parse_either p = either (\m -> Left ("kk_parse: " ++ show m)) Right . P.parse p ""

-- | Run parser and report any error.  Does not delete leading spaces.
kk_parse :: P t -> String -> t
kk_parse p = either (\e -> error e) id . kk_parse_either p

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

kk_lexeme :: P t -> P t
kk_lexeme p = p >>~ P.many P.space

kk_uint :: P Int
kk_uint = do
  digits <- P.many1 P.digit
  return (read digits)

kk_int :: P Int
kk_int = do
  sign <- P.optionMaybe (P.char '-')
  unsigned <- kk_uint
  return (maybe unsigned (const (negate unsigned)) sign)

-- * Note elements parsers

kk_note_name_p :: P Char
kk_note_name_p = P.oneOf "abcdefg"

kk_midi_note_p :: P Int
kk_midi_note_p = P.char 'p' >> kk_uint

kk_rest_p :: P Char
kk_rest_p = P.char 'r'

kk_accidental_p :: P Char
kk_accidental_p = P.oneOf "+-"

kk_char_to_note_number :: Char -> Int
kk_char_to_note_number c = fromMaybe (error "kk_char_to_note_number?") (lookup c (zip "cdefgab" [0, 2, 4, 5, 7, 9, 11]))

kk_char_to_alteration :: Char -> Int
kk_char_to_alteration c = fromMaybe (error "kk_char_to_alteration?") (lookup c (zip "+-" [1, -1]))

-- > map kk_note_number_to_name [0 .. 11]
kk_note_number_to_name :: Int -> String
kk_note_number_to_name k = fromMaybe (error "kk_note_number_to_name?") (lookup k (zip [0..] (words "c c+ d e- e f f+ g a- a b- b")))

kk_named_note_number_p :: P Int
kk_named_note_number_p = do
  nm <- kk_note_name_p
  ac <- P.optionMaybe kk_accidental_p
  return (kk_char_to_note_number nm + maybe 0 kk_char_to_alteration ac)

kk_note_number_p :: P Int
kk_note_number_p = kk_named_note_number_p P.<|> kk_midi_note_p

-- | The octave key can be elided, ordinarily directly after the note name, ie. c2.
kk_modifier_p :: P (Char, Int)
kk_modifier_p = do
  c <- P.optionMaybe (P.oneOf "ovdct")
  n <- kk_int
  return (fromMaybe 'o' c, n)

kk_modifiers_p :: P [(Char, Int)]
kk_modifiers_p = P.many kk_modifier_p

-- * Contextual note

{- | A note where all fields are optional.
If the note number is absent it indicates a rest.
All other fields infer values from the phrase context.
-}
data Kk_Contextual_Note =
  Kk_Contextual_Note
  {kk_contextual_note_number :: Maybe Int
  ,kk_contextual_note_octave :: Maybe Int
  ,kk_contextual_note_volume :: Maybe Int
  ,kk_contextual_note_duration :: Maybe Int
  ,kk_contextual_note_channel :: Maybe Int
  ,kk_contextual_note_time :: Maybe Int}
  deriving (Eq, Ord, Show)

kk_empty_contextual_note :: Kk_Contextual_Note
kk_empty_contextual_note = Kk_Contextual_Note Nothing Nothing Nothing Nothing Nothing Nothing

kk_empty_contextual_rest :: Int -> Kk_Contextual_Note
kk_empty_contextual_rest n = kk_empty_contextual_note {kk_contextual_note_duration = Just n}

{- | If t is set and is at the end time of the previous note print a preceding comma, else print t annotation.

> c = kk_empty_contextual_note {kk_contextual_note_number = Just 0, kk_contextual_time = Just 96}
> map (\t -> kk_contextual_note_pp (t, c)) [0, 96] == ["ct96",", c"]
-}
kk_contextual_note_pp :: (Int, Kk_Contextual_Note) -> String
kk_contextual_note_pp (t', Kk_Contextual_Note n o v d c t) =
  let f i j = maybe "" ((if i == 'o' then id else (i :)) . show) j
      (pre, t'') = if t == Just t' then (", ","") else ("", f 't' t)
  in case n of
          Nothing -> concat [pre, "r", f 'd' d, t'']
          Just k -> concat [pre, kk_note_number_to_name k, f 'o' o, f 'v' v, f 'd' d, f 'c' c, t'']

{- | If the note number is given as p60, then derive octave of and set it, ignoring any modifier.
Note that in KeyKit c3 is p60 or middle c.
-}
kk_contextual_note_p :: P Kk_Contextual_Note
kk_contextual_note_p = do
  n <- fmap Just kk_note_number_p P.<|> (kk_rest_p >> return Nothing)
  m <- kk_modifiers_p
  _ <- P.many P.space
  let get c = lookup c m
      (n', o) =
        case n of
          Just n'' ->
            if n'' > 11
            then
              let (o', n''') = n'' `divMod` 12
              in (Just n''', Just (o' - 2))
            else (n, get 'o')
          Nothing -> (Nothing, Nothing)
  return (Kk_Contextual_Note n' o (get 'v') (get 'd') (get 'c') (get 't'))

kk_contextual_note_is_rest :: Kk_Contextual_Note -> Bool
kk_contextual_note_is_rest = isNothing . kk_contextual_note_number

kk_comma_p :: P Char
kk_comma_p = kk_lexeme (P.char ',')

-- | A contextual note and an is_parallel? indicator.
kk_contextual_phrase_element_p :: P (Kk_Contextual_Note, Bool)
kk_contextual_phrase_element_p = do
  n <- kk_contextual_note_p
  c <- P.optionMaybe kk_comma_p
  return (n, isNothing c)

kk_contextual_phrase_p :: P [(Kk_Contextual_Note, Bool)]
kk_contextual_phrase_p = P.many kk_contextual_phrase_element_p

-- * Note

-- | A note with all fields required.
data Kk_Note =
  Kk_Note
  {kk_note_number :: Int
  ,kk_note_octave :: Int
  ,kk_note_volume :: Int
  ,kk_note_duration :: Int
  ,kk_note_channel :: Int
  ,kk_note_time :: Int}
  deriving (Eq, Ord, Show)

kk_default_note :: Kk_Note
kk_default_note = Kk_Note 60 3 63 96 1 0

kk_note_to_initial_contextual_note :: Kk_Note -> Kk_Contextual_Note
kk_note_to_initial_contextual_note (Kk_Note n o v d c t) =
  let f i j = if i == j then Nothing else Just i
  in Kk_Contextual_Note (Just n) (f o 3) (f v 63) (f d 96) (f c 1) (f t 0)

kk_note_to_contextual_note :: Kk_Note -> Kk_Note -> (Int, Kk_Contextual_Note)
kk_note_to_contextual_note (Kk_Note _ o' v' d' c' t') (Kk_Note n o v d c t) =
  let f i j = if i == j then Nothing else Just i
  in (t' + d', Kk_Contextual_Note (Just n) (f o o') (f v v') (f d d') (f c c') (f t t'))

-- | Elide octave modifier character.
kk_note_pp :: Kk_Note -> String
kk_note_pp (Kk_Note n o v d c t) = printf "%s%dv%dd%dc%dt%d" (kk_note_number_to_name n) o v d c t

kk_decontextualise_note :: Kk_Note -> Bool -> Kk_Contextual_Note -> Either Kk_Note Int
kk_decontextualise_note (Kk_Note _ o v d c t) is_par (Kk_Contextual_Note k' o' v' d' c' t') =
  let t'' = fromMaybe (if is_par then t else t + d) t'
  in case k' of
    Just k'' -> Left (Kk_Note k'' (fromMaybe o o') (fromMaybe v v') (fromMaybe d d') (fromMaybe c c') t'')
    Nothing -> Right t''

data Kk_Phrase = Kk_Phrase { kk_phrase_notes :: [Kk_Note], kk_phrase_length :: Int } deriving (Eq, Show)

-- | This should, but does not, append a trailing rest as required.
kk_phrase_pp :: Kk_Phrase -> String
kk_phrase_pp (Kk_Phrase n _) = unwords (map kk_note_pp n)

-- | Rests are elided, their duration is accounted for in the time of the following notetaken into account.
kk_decontextualise_phrase :: [(Kk_Contextual_Note, Bool)] -> Kk_Phrase
kk_decontextualise_phrase =
  let f r c p l =
        case l of
          [] -> Kk_Phrase (reverse r) (kk_note_time c + kk_note_duration c)
          (n,p'):l' ->
            case kk_decontextualise_note c p n of
              Left c' -> f (c' : r) c' p' l'
              Right t' -> f r (c {kk_note_time = t'}) p' l'
  in f [] kk_default_note True

-- | In addition to contextual note give end time of previous note, to allow for sequence (comma) notation.
kk_recontextualise_phrase :: Kk_Phrase -> [(Int, Kk_Contextual_Note)]
kk_recontextualise_phrase p =
  let f n0 n =
        case n of
          [] -> []
          n1 : n' -> kk_note_to_contextual_note n0 n1 : f n1 n'
  in case p of
    Kk_Phrase [] l -> [(0, kk_empty_contextual_rest l)]
    Kk_Phrase (n1 : n') _ ->
      let c1 = kk_note_to_initial_contextual_note n1
      in (0, c1) : f n1 n'

{- | Read KeyKit phrase constant.

> let rw = (\p -> (kk_phrase_pp p, kk_phrase_length p)) . kk_phrase_read
> rw "c" == ("c3v63d96c1t0",96)
> rw "c, r" == ("c3v63d96c1t0",192)
> rw "c, r, c3, r, p60" == ("c3v63d96c1t0 c3v63d96c1t192 c3v63d96c1t384",480)
> rw "c, e, g" == ("c3v63d96c1t0 e3v63d96c1t96 g3v63d96c1t192",288)
> rw "c2" == rw "co2"
-}
kk_phrase_read :: String -> Kk_Phrase
kk_phrase_read = kk_decontextualise_phrase . kk_parse kk_contextual_phrase_p

{- | Re-contextualise and print phrase.

> rw = kk_phrase_print . kk_phrase_read
> rw_id i = rw i == i
> rw_id "c"
> rw_id "c e g"
> rw_id "c , e , g"
> rw_id "c e g , c f a , c e g , c e- g"
> rw_id "c , e , g c4t384"
> rw "c, r, c3, r, p60" == "c ct192 ct384"
> rw "c , e , g c4t288" == "c , e , g , c4"
> rw "c r" == "c" -- ?
-}
kk_phrase_print :: Kk_Phrase -> String
kk_phrase_print = unwords . map kk_contextual_note_pp . kk_recontextualise_phrase
