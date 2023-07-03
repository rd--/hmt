{- | /Bel(R)/ is a simplified form of the /Bel/ notation described in:

- Bernard Bel.
  \"Time and musical structures\".
  /Interface (Journal of New Music Research)/
  Volume 19, Issue 2-3, 1990.
  (<http://hal.archives-ouvertes.fr/hal-00134160>)

- Bernard Bel.
  \"Two algorithms for the instantiation of structures of musical objects\".
  Centre National de la Recherche Scientifique, 1992. /GRTC 458/
  (<http://www.lpl.univ-aix.fr/~belbernard/music/2algorithms.pdf>)

For details see <http://rohandrape.net/?t=hmt-texts>.
-}

module Music.Theory.Time.Bel1990.R where

import Control.Monad {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Text.Parsec as Parsec {- parsec -}

import qualified Music.Theory.List as List {- hmt-base -}
import Music.Theory.Parse (P) {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import Music.Theory.Time {- hmt -}

-- * Bel

{- | Types of 'Par' nodes. -}
data Par_Mode = Par_Left | Par_Right | Par_Min | Par_Max | Par_None
  deriving (Eq, Show)

{- | The different 'Par' modes are indicated by bracket types. -}
par_mode_brackets :: Par_Mode -> (String,String)
par_mode_brackets m =
    case m of
      Par_Left -> ("(",")")
      Par_Right -> ("~(",")")
      Par_Min -> ("~{","}")
      Par_Max -> ("{","}")
      Par_None -> ("[","]")

{- | Inverse of par_mode_brackets -}
par_mode_kind :: (String, String) -> Par_Mode
par_mode_kind brk =
  case brk of
    ("{","}") -> Par_Max
    ("~{","}") -> Par_Min
    ("(",")") -> Par_Left
    ("~(",")") -> Par_Right
    ("[","]") -> Par_None
    _ -> error "par_mode_kind: incoherent par"

bel_brackets_match :: (Char,Char) -> Bool
bel_brackets_match (open,close) =
    case (open,close) of
      ('{','}') -> True
      ('(',')') -> True
      ('[',']') -> True
      _ -> False

{- | Terms are the leaf nodes of the temporal structure. -}
data Term a = Value a | Rest | Continue
  deriving (Eq,Show)

{- | Value of Term, else Nothing -}
term_value :: Term t -> Maybe t
term_value t =
  case t of
    Value x -> Just x
    _ -> Nothing

{- | Recursive temporal structure.
The duration of a 'Term' is the reciprocal of the 'Tempo' that is in place at the 'Term'.
-}
data Bel a =
  Node (Term a) -- ^ Leaf node
  | Iso (Bel a) -- ^ Isolate
  | Seq (Bel a) (Bel a) -- ^ Sequence
  | Par Par_Mode (Bel a) (Bel a) -- ^ Parallel
  | Mul Tempo -- ^ Tempo multiplier
  deriving (Eq,Show)

{- | Given a Par mode, generate either: 1. an Iso, 2. a Par, 3. a series of nested Par. -}
par_of :: Par_Mode -> [Bel a] -> Bel a
par_of m l =
  case l of
    [] -> error "par_of: null"
    [e] -> Iso e
    lhs : rhs : [] -> Par m lhs rhs
    e : l' -> Par m e (par_of m l')

{- | Pretty printer for 'Bel', given pretty printer for the term type.
Note this does not write nested Par nodes in their simplified form.
-}
bel_pp :: (a -> String) -> Bel a -> String
bel_pp f b =
    case b of
      Node Rest -> "-"
      Node Continue -> "_"
      Node (Value c) -> f c
      Iso b' -> List.bracket_l ("{","}") (bel_pp f b')
      Seq p q -> concat [bel_pp f p,bel_pp f q]
      Par m p q ->
          let pq = concat [bel_pp f p,",",bel_pp f q]
          in List.bracket_l (par_mode_brackets m) pq
      Mul n -> concat ["*",Show.rational_pp n]

{- | 'bel_pp' of 'return'. -}
bel_char_pp :: Bel Char -> String
bel_char_pp = bel_pp return

{- | Analyse a Par node giving (duration,LHS-tempo-*,RHS-tempo-*).

>>> par_analyse 1 Par_Left (nseq "cd") (nseq "efg") == (2,1,3/2)
True

>>> par_analyse 1 Par_Right (nseq "cd") (nseq "efg") == (3,2/3,1)
True

>>> par_analyse 1 Par_Min (nseq "cd") (nseq "efg") == (2,1,3/2)
True

>>> par_analyse 1 Par_Max (nseq "cd") (nseq "efg") == (3,2/3,1)
True

>>> par_analyse 1 Par_None (nseq "cd") (nseq "efg") == (3,1,1)
True
-}
par_analyse :: Tempo -> Par_Mode -> Bel a -> Bel a -> (Rational,Rational,Rational)
par_analyse t m p q =
    let (_,d_p) = bel_tdur t p
        (_,d_q) = bel_tdur t q
    in case m of
         Par_Left -> (d_p,1,d_q / d_p)
         Par_Right -> (d_q,d_p / d_q,1)
         Par_Min -> let r = min d_p d_q in (r,d_p / r,d_q / r)
         Par_Max -> let r = max d_p d_q in (r,d_p / r,d_q / r)
         Par_None -> (max d_p d_q,1,1)

{- | Duration element of 'par_analyse'. -}
par_dur :: Tempo -> Par_Mode -> Bel a -> Bel a -> Rational
par_dur t m p q =
    let (d,_,_) = par_analyse t m p q
    in d

{- | Calculate final tempo and duration of 'Bel'. -}
bel_tdur :: Tempo -> Bel a -> (Tempo,Rational)
bel_tdur t b =
    case b of
      Node _ -> (t,1 / t)
      Iso b' -> (t,snd (bel_tdur t b'))
      Seq p q ->
          let (t_p,d_p) = bel_tdur t p
              (t_q,d_q) = bel_tdur t_p q
          in (t_q,d_p + d_q)
      Par m p q -> (t,par_dur t m p q)
      Mul n -> (t * n,0)

{- | 'snd' of 'bel_tdur'. -}
bel_dur :: Tempo -> Bel a -> Rational
bel_dur t = snd . bel_tdur t

-- * Linearisation

{- | Voices are named as a sequence of left and right directions within nested 'Par' structures.
l is left and r is right.
-}
type Voice = [Char]

{- | Linear state.
'Time' is the start time of the term.
'Tempo' is the active tempo & therefore the reciprocal of the duration.
'Voice' is the part label.
-}
type L_St = (Time, Tempo, Voice)

{- | Linear term. -}
type L_Term a = (L_St,Term a)

{- | Start time of 'L_Term'. -}
lterm_time :: L_Term a -> Time
lterm_time ((st,_,_),_) = st

{- | Duration of 'L_Term' (reciprocal of tempo). -}
lterm_duration :: L_Term a -> Time
lterm_duration ((_,tm,_),_) = 1 / tm

{- | End time of 'L_Term'. -}
lterm_end_time :: L_Term a -> Time
lterm_end_time e = lterm_time e + lterm_duration e

{- | Voice of 'L_Term'. -}
lterm_voice :: L_Term t -> Voice
lterm_voice ((_,_,vc),_) = vc

{- | Term of L_Term -}
lterm_term :: L_Term t -> Term t
lterm_term (_,t) = t

{- | Value of Term of L_Term -}
lterm_value :: L_Term t -> Maybe t
lterm_value = term_value . lterm_term

{- | Linear form of 'Bel', an ascending sequence of 'L_Term'. -}
type L_Bel a = [L_Term a]

{- | Linearise 'Bel' given initial 'L_St', ascending by construction. -}
bel_linearise :: L_St -> Bel a -> (L_Bel a,L_St)
bel_linearise l_st b =
    let (st,tm,vc) = l_st
    in case b of
         Node e -> ([(l_st,e)],(st + 1/tm,tm,vc))
         Iso p ->
             let (p',(st',_,_)) = bel_linearise l_st p
             in (p',(st',tm,vc))
         Seq p q ->
             let (p',l_st') = bel_linearise l_st p
                 (q',l_st'') = bel_linearise l_st' q
             in (p' ++ q',l_st'')
         Par m p q ->
             let (du,p_m,q_m) = par_analyse tm m p q
                 (p',_) = bel_linearise (st,tm * p_m,'l':vc) p
                 (q',_) = bel_linearise (st,tm * q_m,'r':vc) q
             in (p' `lbel_merge` q',(st + du,tm,vc))
         Mul n -> ([],(st,tm * n,vc))

{- | Merge two ascending 'L_Bel'. -}
lbel_merge :: L_Bel a -> L_Bel a -> L_Bel a
lbel_merge = List.merge_on lterm_time

{- | Set of unique 'Tempo' at 'L_Bel'. -}
lbel_tempi :: L_Bel a -> [Tempo]
lbel_tempi = nub . sort . map (\((_,t,_),_) -> t)

{- | Multiply 'Tempo' by /n/, and divide 'Time' by /n/. -}
lbel_tempo_mul :: Rational -> L_Bel a -> L_Bel a
lbel_tempo_mul n = map (\((st,tm,vc),e) -> ((st / n,tm * n,vc),e))

{- | The multiplier that will normalise an L_Bel value.
After normalisation all start times and durations are integral.
-}
lbel_normalise_multiplier :: L_Bel t -> Rational
lbel_normalise_multiplier b =
  let t = lbel_tempi b
      n = foldl1 lcm (map denominator t) % 1
      m = foldl1 lcm (map (numerator . (* n)) t) % 1
  in n / m

{- | Calculate and apply L_Bel normalisation multiplier. -}
lbel_normalise :: L_Bel a -> L_Bel a
lbel_normalise b = lbel_tempo_mul (lbel_normalise_multiplier b) b

{- | All leftmost voices are re-written to the last non-left turning point.

>>> map voice_normalise ["","l","ll","lll"] == replicate 4 ""
True

>>> voice_normalise "lllrlrl"
"rlrl"
-}
voice_normalise :: Voice -> Voice
voice_normalise = dropWhile (== 'l')

{- | '==' 'on' 'voice_normalise' -}
voice_eq :: Voice -> Voice -> Bool
voice_eq = (==) `on` voice_normalise

{- | Unique 'Voice's at 'L_Bel'. -}
lbel_voices :: L_Bel a -> [Voice]
lbel_voices =
    sortOn reverse .
    nub .
    map (\((_,_,v),_) -> voice_normalise v)

{- | The duration of 'L_Bel'. -}
lbel_duration :: L_Bel a -> Time
lbel_duration b =
    let l = last (List.group_on lterm_time b)
    in maximum (map (\((st,tm,_),_) -> st + recip tm) l)

{- | Locate an 'L_Term' that is active at the indicated 'Time' and in the indicated 'Voice'. -}
lbel_lookup :: (Time,Voice) -> L_Bel a -> Maybe (L_Term a)
lbel_lookup (st,vc) =
    let f ((st',tm,vc'),_) = (st >= st' && st < st' + (1 / tm)) &&
                             vc `voice_eq` vc'
    in find f

{- | Calculate grid (phase diagram) for 'L_Bel'. -}
lbel_grid :: L_Bel a -> [[Maybe (Term a)]]
lbel_grid l =
    let n = lbel_normalise l
        v = lbel_voices n
        d = lbel_duration n
        trs st ((st',_,_),e) = if st == st' then e else Continue
        get vc st = fmap (trs st) (lbel_lookup (st,vc) n)
        f vc = map (get vc) [0 .. d - 1]
    in map f v

{- | 'lbel_grid' of 'bel_linearise'. -}
bel_grid :: Bel a -> [[Maybe (Term a)]]
bel_grid b =
    let (l,_) = bel_linearise (0,1,[]) b
    in lbel_grid l

{- | /Bel/ type phase diagram for 'Bel' of 'Char'.
Optionally print whitespace between columns.
-}
bel_ascii :: Bool -> Bel Char -> String
bel_ascii opt =
    let f e = case e of
                Nothing -> ' '
                Just Rest -> '-'
                Just Continue -> '_'
                Just (Value c) -> c
        g = if opt then intersperse ' ' else id
    in unlines . map (g . map f) . bel_grid

{- | 'putStrLn' of 'bel_ascii'. -}
bel_ascii_pr :: Bel Char -> IO ()
bel_ascii_pr = putStrLn . ('\n' :) . bel_ascii True

-- * Combinators

{- | Infix form for 'Seq'. -}
(~>) :: Bel a -> Bel a -> Bel a
p ~> q = Seq p q

{- | 'foldl1' of 'Seq'.

>>> lseq [Node Rest]
Node Rest

>>> lseq [Node Rest,Node Continue]
Seq (Node Rest) (Node Continue)
-}
lseq :: [Bel a] -> Bel a
lseq = foldl1 Seq

{- | 'Node' of 'Value'. -}
node :: a -> Bel a
node = Node . Value

{- | 'lseq' of 'Node' -}
nseq :: [a] -> Bel a
nseq = lseq . map node

{- | Variant of 'nseq' where @_@ is read as 'Continue' and @-@ as 'Rest'. -}
cseq :: String -> Bel Char
cseq =
    let f c = case c of
                '_' -> Continue
                '-' -> Rest
                _ -> Value c
    in foldl1 Seq . map (Node . f)

{- | 'Par' of 'Par_Max', this is the default 'Par_Mode'. -}
par :: Bel a -> Bel a -> Bel a
par = Par Par_Max

{- | 'Node' of 'Rest'. -}
rest :: Bel a
rest = Node Rest

{- | 'lseq' of 'replicate' of 'rest'. -}
nrests :: Integral n => n -> Bel a
nrests n = lseq (genericReplicate n rest)

{- | Verify that 'bel_char_pp' of 'bel_char_parse' is 'id'. -}
bel_parse_pp_ident :: String -> Bool
bel_parse_pp_ident s = bel_char_pp (bel_char_parse s) == s

{- | Run 'bel_char_parse', and print both 'bel_char_pp' and 'bel_ascii'.

> bel_ascii_pp "{i{ab,c[d,oh]e,sr{p,qr}},{jk,ghjkj}}"

@
Bel(R): "{i{ab,{c[d,oh]e,sr{p,qr}}},{jk,ghjkj}}", Dur: 5/1

i _ a _ _ _ b _ _ _
    c _ d _     e _
        o _ h _    
    s _ r _ p _ _ _
            q _ r _
j _ _ _ _ k _ _ _ _
g _ h _ j _ k _ j _
@
-}
bel_ascii_pp :: String -> IO ()
bel_ascii_pp s = do
  let p = bel_char_parse s
  putStrLn (concat ["\nBel(R): \"",bel_char_pp p,"\", Dur: ",Show.rational_pp (bel_dur 1 p),""])
  bel_ascii_pr p

-- * Parsing

{- | Parse 'Rest' 'Term'.

>>> Parsec.parse p_rest "" "-"
Right Rest
-}
p_rest :: P (Term a)
p_rest = fmap (const Rest) (Parsec.char '-')

{- | Parse 'Rest' 'Term'.

>>> Parsec.parse p_nrests "" "3"
Right (Seq (Seq (Node Rest) (Node Rest)) (Node Rest))
-}
p_nrests :: P (Bel a)
p_nrests = fmap nrests p_non_negative_integer

{- | Parse 'Continue' 'Term'.

>>> Parsec.parse p_continue "" "_"
Right Continue
-}
p_continue :: P (Term a)
p_continue = fmap (const Continue) (Parsec.char '_')

{- | Parse 'Char' 'Value' 'Term'.

>>> Parsec.parse p_char_value "" "a"
Right (Value 'a')
-}
p_char_value :: P (Term Char)
p_char_value = fmap Value Parsec.lower

{- | Parse 'Char' 'Term'.

>>> Parsec.parse (Parsec.many1 p_char_term) "" "-_a"
Right [Rest,Continue,Value 'a']
-}
p_char_term :: P (Term Char)
p_char_term = Parsec.choice [p_rest,p_continue,p_char_value]

{- | Parse 'Char' 'Node'.

>>> Parsec.parse (Parsec.many1 p_char_node) "" "-_a"
Right [Node Rest,Node Continue,Node (Value 'a')]
-}
p_char_node :: P (Bel Char)
p_char_node = fmap Node p_char_term

{- | Parse non-negative 'Integer'.

>>> Parsec.parse p_non_negative_integer "" "3"
Right 3
-}
p_non_negative_integer :: P Integer
p_non_negative_integer = fmap read (Parsec.many1 Parsec.digit)

{- | Parse non-negative 'Rational'.

>>> Parsec.parse (p_non_negative_rational `Parsec.sepBy` (Parsec.char ',')) "" "3%5,2/3"
Right [3 % 5,2 % 3]
-}
p_non_negative_rational :: P Rational
p_non_negative_rational = do
  n <- p_non_negative_integer
  _ <- Parsec.oneOf "%/"
  d <- p_non_negative_integer
  return (n % d)

{- | Parse non-negative 'Double'.

>>> Parsec.parse p_non_negative_double "" "3.5"
Right 3.5

>>> Parsec.parse (p_non_negative_double `Parsec.sepBy` (Parsec.char ',')) "" "3.5,7.2,1.0"
Right [3.5,7.2,1.0]
-}
p_non_negative_double :: P Double
p_non_negative_double = do
  a <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  b <- Parsec.many1 Parsec.digit
  return (read (a ++ "." ++ b))

{- | Parse non-negative number as 'Rational'.

>>> Parsec.parse (p_non_negative_number `Parsec.sepBy` (Parsec.char ',')) "" "7%2,3.5,3"
Right [7 % 2,7 % 2,3 % 1]
-}
p_non_negative_number :: P Rational
p_non_negative_number =
    Parsec.choice [Parsec.try p_non_negative_rational
             ,Parsec.try (fmap toRational p_non_negative_double)
             ,Parsec.try (fmap toRational p_non_negative_integer)]

{- | Parse 'Mul'.

>>> Parsec.parse (Parsec.many1 p_mul) "" "/3*3/2"
Right [Mul (1 % 3),Mul (3 % 2)]
-}
p_mul :: P (Bel a)
p_mul = do
  op <- Parsec.oneOf "*/"
  n <- p_non_negative_number
  let n' = case op of
             '*' -> n
             '/' -> recip n
             _ -> error "p_mul"
  return (Mul n')

{- | Given parser for 'Bel' /a/, generate 'Iso' parser. -}
p_iso :: P (Bel a) -> P (Bel a)
p_iso f = do
  open <- Parsec.oneOf "{(["
  iso <- Parsec.many1 f
  close <- Parsec.oneOf "})]"
  when (not (bel_brackets_match (open,close))) (error "p_iso: open/close mismatch")
  return (Iso (lseq iso))

{- | 'p_iso' of 'p_char_bel'.

>>> Parsec.parse p_char_iso "" "{abcde}"
Right (Iso (Seq (Seq (Seq (Seq (Node (Value 'a')) (Node (Value 'b'))) (Node (Value 'c'))) (Node (Value 'd'))) (Node (Value 'e'))))
-}
p_char_iso :: P (Bel Char)
p_char_iso = p_iso p_char_bel

{- | Given parser for 'Bel' /a/, generate 'Par' parser. -}
p_par :: P (Bel a) -> P (Bel a)
p_par f = do
  tilde <- Parsec.optionMaybe (Parsec.char '~')
  open <- Parsec.oneOf "{(["
  items <- Parsec.sepBy (Parsec.many1 f) (Parsec.char ',')
  close <- Parsec.oneOf "})]"
  let m = par_mode_kind (List.mcons tilde [open], [close])
  return (par_of m (map lseq items))

{- | 'p_par' of 'p_char_bel'.

>>> p = Parsec.parse p_char_par ""
>>> p "{ab,{c,de}}" == p "{ab,c,de}"
True

>>> p "{ab,~(c,de)}"
Right (Par Par_Max (Seq (Node (Value 'a')) (Node (Value 'b'))) (Par Par_Right (Node (Value 'c')) (Seq (Node (Value 'd')) (Node (Value 'e')))))
-}
p_char_par :: P (Bel Char)
p_char_par = p_par p_char_bel

{- | Parse 'Bel' 'Char'.

>>> Parsec.parse (Parsec.many1 p_char_bel) "" "-_a*3"
Right [Node Rest,Node Continue,Node (Value 'a'),Mul (3 % 1)]
-}
p_char_bel :: P (Bel Char)
p_char_bel = Parsec.choice [Parsec.try p_char_par,p_char_iso,p_mul,p_nrests,p_char_node]

{- | Run parser for 'Bel' of 'Char'. -}
bel_char_parse :: String -> Bel Char
bel_char_parse s =
    either
    (\e -> error ("bel_parse failed\n" ++ show e))
    lseq
    (Parsec.parse (Parsec.many1 p_char_bel) "" s)
