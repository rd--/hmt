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

For patterns without tempo indications, the two notations should give
equivalent phase diagrams, for instance (Bel 1990, §11, p.24):

> > bel_ascii_pp "ab{ab,cde}cd"
>
> Bel(R): "ab{ab,cde}cd", Dur: 7
>
> a _ b _ a _ _ b _ _ c _ d _
>         c _ d _ e _        

and:

> > bel_ascii_pp "{a{bc,def},ghijk}"
>
> Bel(R): "{a{bc,def},ghijk}", Dur: 5
>
> a _ _ _ _ _ _ _ _ _ b _ _ _ _ _ _ _ _ _ _ _ _ _ _ c _ _ _ _ _ _ _ _ _ _ _ _ _ _
>                     d _ _ _ _ _ _ _ _ _ e _ _ _ _ _ _ _ _ _ f _ _ _ _ _ _ _ _ _
> g _ _ _ _ _ _ _ h _ _ _ _ _ _ _ i _ _ _ _ _ _ _ j _ _ _ _ _ _ _ k _ _ _ _ _ _ _

The /Bel/ notation allows /n/-ary parallel structures,
ie. @{a_bcd_e,a_f_gh_,ji_a_i_}@ (Bel 1992, p.29), however /Bel(R)/
allows only binary structures.  The parallel interpretation rules are
associative:

> > bel_ascii_pp "{a_bcd_e,{a_f_gh_,ji_a_i_}}"
>
> Bel(R): "{a_bcd_e,{a_f_gh_,ji_a_i_}}", Dur: 7
>
> a _ b c d _ e
> a _ f _ g h _
> j i _ a _ i _

/Bel(R)/ does allow unary parallel structures (see 'Iso'), which can
be used to /isolate/ tempo changes:

> > bel_ascii_pp "ab{*2cd}ef{*2/3gh}ij"
>
> Bel(R): "ab{*2cd}ef{*2/3gh}ij", Dur: 10
>
> a _ b _ c d e _ f _ g _ _ h _ _ i _ j _

Patterns with tempo indications have completely different meanings in
/Bel/ and /Bel(R)/, though in both cases parallel nodes delimit the
scope of tempo markings.

/Bel(R)/ replaces the @\/n@ notation for explicit tempo marks with a
@*n@ notation to indicate a tempo multiplier, and a set of bracketing
notations to specify interpretation rules for parallel (concurrent)
temporal structures.

The tempo indication @\/1@ in the expression @ab{\/1ab,cde}cd@
(Bel 1990, p.24) requires that the inner @ab@ have the same tempo as
the outer @ab@, which is implicitly @\/1@.  Setting the tempo of one
part of a parallel structure requires assigning a tempo to the other
part in order that the two parts have equal duration.  Here the tempo
assigned to @cde@ is @\/1.5@, but since fractional tempi are not
allowed the expression is re-written as @\/2ab{\/2ab,\/3cde}\/2cd@.

Importantly the explicit tempo indications make it possible to write
syntactically correct expressions in /Bel/ that do not have a coherent
interpretation, ie. @{\/1ab,\/1cde}@.  Determining if a coherent set
of tempos can be assigned, and assigning these tempos, is the object
of the interpretation system.

In comparison, all syntactically valid /Bel(R)/ strings have an
interpretation.  The expression @{*1ab,*1cde}@ is trivially equal to
@{ab,cde}@, and tempo marks in parallel parts do not interact:

> > bel_ascii_pp "{a*2b,*3c/2d/3e}"
>
> Bel(R): "{a*2b,*3c*1/2d*1/3e}", Dur: 3
>
> a _ _ _ _ _ b _ _
> c d _ e _ _ _ _ _

Here @a@ is twice the duration of @b@, and @e@ is three times the
duration of @d@, which is twice the duration of @c@ (in /Bel(R)/ @\/n@
is equivalent to @*1\/n@).  The duration of any /Bel(R)/ expression
can be calculated directly, given an initial 'Tempo':

> bel_dur 1 (bel_char_parse "a*2b") == 3/2
> bel_dur 1 (bel_char_parse "*3c/2d/3e") == 3

Therefore in the composite expression the left part is slowed by a
factor of two to align with the right part.

The /Bel/ string @ab{\/1ab,cde}cd@ can be re-written in /Bel(R)/ as
either @ab~{ab,cde}cd@ or @ab(ab,cde)cd@.  The absolute tempo
indication is replaced by notations giving alternate modes of
interpretation for the parallel structure.

In the first case the @~@ indicates the /opposite/ of the normal rule
for parallel nodes.  The normal rule is the same as for /Bel/ and is
that the duration of the whole is equal to duration of the longer of
the two parts.  The @~@ inverts this so that the whole has the
duration of the shorter of the two parts, and the longer part is
scaled to have equal duration.

In the second case the parentheses @()@ replacing the braces @{}@
indicates that the duration of the whole is equal to the duration of
the left side, and that the right is to be scaled.  Similarly, a @~@
preceding parentheses indicates the duration of the whole should be
the duration of the right side, and the left scaled.

> > bel_ascii_pp "ab~{ab,cde}cd"
>
> Bel(R): "ab~{ab,cde}cd", Dur: 6
>
> a _ _ b _ _ a _ _ b _ _ c _ _ d _ _
>             c _ d _ e _            

There is one other parallel mode that has no equivalent in /Bel/
notation.  It is a mode that does not scale either part, leaving a
/hole/ at the end of the shorter part, and is indicated by square
brackets:

> > bel_ascii_pp "ab[ab,cde]cd"
>
> Bel(R): "ab[ab,cde]cd", Dur: 7
>
> a b a b   c d
>     c d e    

The /Bel/ string @\/2abc\/3de@ (Bel 1992, p.53) can be written as
@*2abc*1/2*3de@, or equivalently as @*2abc*3/2de@:

> > bel_ascii_pp "*2abc*3/2de"
>
> Bel(R): "*2abc*3/2de", Dur: 13/6
>
> a _ _ b _ _ c _ _ d _ e _

It can also be written using the shorthand notation for rest
sequences, where an integer /n/ indicates a sequence of /n/ rests, as:

> > bel_ascii_pp "(9,abc)(4,de)"
>
> Bel(R): "(---------,abc)(----,de)", Dur: 13
>
> - - - - - - - - - - - - -
> a _ _ b _ _ c _ _ d _ e _

In the /Bel/ string @{ab{/3abc,de},fghijk}@ (Bel 1992, p.20) the tempo
indication does not change the inter-relation of the parts but rather
scales the parallel node altogether, and can be re-written in /Bel(R)/
notation as:

> > bel_ascii_pp "{ab*3{abc,de},fghijk}"
>
> Bel(R): "{ab*3{abc,de},fghijk}", Dur: 6
>
> a _ _ _ _ _ b _ _ _ _ _ a _ b _ c _
>                         d _ _ e _ _
> f _ _ g _ _ h _ _ i _ _ j _ _ k _ _

Curiously the following example (Bel 1990, p. 24) does not correspond
to the phase diagram given:

> > bel_ascii_pp "{i{ab,cde},jk}"
>
> Bel(R): "{i{ab,cde},jk}", Dur: 4
>
> i _ a _ _ b _ _
>     c _ d _ e _
> j _ _ _ k _ _ _

The paper assigns tempi of @\/6@ to both @i@ and @ab@, which in
/Bel(R)/ could be written:

> > bel_ascii_pp "{i~{ab,cde},jk}"
>
> Bel(R): "{i~{ab,cde},jk}", Dur: 3
>
> i _ _ _ _ _ a _ _ _ _ _ b _ _ _ _ _
>             c _ _ _ d _ _ _ e _ _ _
> j _ _ _ _ _ _ _ _ k _ _ _ _ _ _ _ _

-}

module Music.Theory.Time.Bel1990.R where

import Control.Monad {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Ratio {- base -}
import qualified Text.ParserCombinators.Parsec as P {- parsec -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Math as T

-- * Bel

-- | Types of 'Par' nodes.
data Par_Mode = Par_Left | Par_Right
              | Par_Min | Par_Max
              | Par_None
              deriving (Eq,Show)

-- | The different 'Par' modes are indicated by bracket types.
par_mode_brackets :: Par_Mode -> (String,String)
par_mode_brackets m =
    case m of
      Par_Left -> ("(",")")
      Par_Right -> ("~(",")")
      Par_Min -> ("~{","}")
      Par_Max -> ("{","}")
      Par_None -> ("[","]")

bel_brackets_match :: (Char,Char) -> Bool
bel_brackets_match (open,close) =
    case (open,close) of
      ('{','}') -> True
      ('(',')') -> True
      ('[',']') -> True
      _ -> False

-- | Tempo is rational.  The duration of a 'Term' is the reciprocal of
-- the 'Tempo' that is in place at the 'Term'.
type Tempo = Rational

-- | Terms are the leaf nodes of the temporal structure.
data Term a = Value a
            | Rest
            | Continue
           deriving (Eq,Show)

-- | Recursive temporal structure.
data Bel a = Node (Term a) -- ^ Leaf node
           | Iso (Bel a) -- ^ Isolate
           | Seq (Bel a) (Bel a) -- ^ Sequence
           | Par Par_Mode (Bel a) (Bel a) -- ^ Parallel
           | Mul Tempo -- ^ Tempo multiplier
           deriving (Eq,Show)

-- | Pretty printer for 'Bel', given pretty printer for the term type.
bel_pp :: (a -> String) -> Bel a -> String
bel_pp f b =
    case b of
      Node Rest -> "-"
      Node Continue -> "_"
      Node (Value c) -> f c
      Iso b' -> T.bracket_l ("{","}") (bel_pp f b')
      Seq p q -> concat [bel_pp f p,bel_pp f q]
      Par m p q ->
          let pq = concat [bel_pp f p,",",bel_pp f q]
          in T.bracket_l (par_mode_brackets m) pq
      Mul n -> concat ["*",T.rational_pp n]

-- | 'bel_pp' of 'return'.
bel_char_pp :: Bel Char -> String
bel_char_pp = bel_pp return

-- | Analyse a Par node giving (duration,LHS-tempo-*,RHS-tempo-*).
--
-- > par_analyse 1 Par_Left (nseq "cd") (nseq "efg") == (2,1,3/2)
-- > par_analyse 1 Par_Right (nseq "cd") (nseq "efg") == (3,2/3,1)
-- > par_analyse 1 Par_Min (nseq "cd") (nseq "efg") == (2,1,3/2)
-- > par_analyse 1 Par_Max (nseq "cd") (nseq "efg") == (3,2/3,1)
-- > par_analyse 1 Par_None (nseq "cd") (nseq "efg") == (3,1,1)
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

-- | Duration element of 'par_analyse'.
par_dur :: Tempo -> Par_Mode -> Bel a -> Bel a -> Rational
par_dur t m p q =
    let (d,_,_) = par_analyse t m p q
    in d

-- | Calculate final tempo and duration of 'Bel'.
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

-- | 'snd' of 'bel_tdur'.
bel_dur :: Tempo -> Bel a -> Rational
bel_dur t = snd . bel_tdur t

-- * Linearisation

-- | Time point.
type Time = Rational

-- | Voices are named as a sequence of left and right directions
-- within nested 'Par' structures.
type Voice = [Char]

-- | Linear state.  'Time' is the start time of the term, 'Tempo' is
-- the active tempo & therefore the reciprocal of the duration,
-- 'Voice' is the part label.
type L_St = (Time,Tempo,Voice)

-- | Linear term.
type L_Term a = (L_St,Term a)

-- | Start time of 'L_Term'.
lterm_time :: L_Term a -> Time
lterm_time ((st,_,_),_) = st

-- | Duration of 'L_Term' (reciprocal of tempo).
lterm_duration :: L_Term a -> Time
lterm_duration ((_,tm,_),_) = 1 / tm

-- | End time of 'L_Term'.
lterm_end_time :: L_Term a -> Time
lterm_end_time e = lterm_time e + lterm_duration e

-- | Linear form of 'Bel', an ascending sequence of 'L_Term'.
type L_Bel a = [L_Term a]

-- | Linearise 'Bel' given initial 'L_St', ascending by construction.
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

-- | Merge two ascending 'L_Bel'.
lbel_merge :: L_Bel a -> L_Bel a -> L_Bel a
lbel_merge = T.merge_by (compare `on` lterm_time)

-- | Set of unique 'Tempo' at 'L_Bel'.
lbel_tempi :: L_Bel a -> [Tempo]
lbel_tempi = nub . sort . map (\((_,t,_),_) -> t)

-- | Multiply 'Tempo' by /n/, and divide 'Time' by /n/.
lbel_tempo_mul :: Rational -> L_Bel a -> L_Bel a
lbel_tempo_mul n = map (\((st,tm,vc),e) -> ((st / n,tm * n,vc),e))

-- | After normalisation all start times and durations are integral.
lbel_normalise :: L_Bel a -> L_Bel a
lbel_normalise b =
    let t = lbel_tempi b
        n = foldl1 lcm (map denominator t) % 1
        m = foldl1 lcm (map numerator (map (* n) t)) % 1
    in lbel_tempo_mul (n / m) b

-- | All leftmost voices are re-written to the last non-left turning point.
--
-- > map voice_normalise ["","l","ll","lll"] == replicate 4 ""
-- > voice_normalise "lllrlrl" == "rlrl"
voice_normalise :: Voice -> Voice
voice_normalise = dropWhile (== 'l')

-- | '==' 'on' 'voice_normalise'
voice_eq :: Voice -> Voice -> Bool
voice_eq = (==) `on` voice_normalise

-- | Unique 'Voice's at 'L_Bel'.
lbel_voices :: L_Bel a -> [Voice]
lbel_voices =
    sortBy (compare `on` reverse) .
    nub .
    map (\((_,_,v),_) -> voice_normalise v)

-- | The duration of 'L_Bel'.
lbel_duration :: L_Bel a -> Time
lbel_duration b =
    let l = last (groupBy ((==) `on` lterm_time) b)
    in maximum (map (\((st,tm,_),_) -> st + recip tm) l)

-- | Locate an 'L_Term' that is active at the indicated 'Time' and in
-- the indicated 'Voice'.
lbel_lookup :: (Time,Voice) -> L_Bel a -> Maybe (L_Term a)
lbel_lookup (st,vc) =
    let f ((st',tm,vc'),_) = (st >= st' && st < st' + (1 / tm)) &&
                             vc `voice_eq` vc'
    in find f

-- | Calculate grid (phase diagram) for 'L_Bel'.
lbel_grid :: L_Bel a -> [[Maybe (Term a)]]
lbel_grid l =
    let n = lbel_normalise l
        v = lbel_voices n
        d = lbel_duration n
        trs st ((st',_,_),e) = if st == st' then e else Continue
        get vc st = fmap (trs st) (lbel_lookup (st,vc) n)
        f vc = map (get vc) [0 .. d - 1]
    in map f v

-- | 'lbel_grid' of 'bel_linearise'.
bel_grid :: Bel a -> [[Maybe (Term a)]]
bel_grid b =
    let (l,_) = bel_linearise (0,1,[]) b
    in lbel_grid l

-- | /Bel/ type phase diagram for 'Bel' of 'Char'.  Optionally print
-- whitespace between columns.
bel_ascii :: Bool -> Bel Char -> String
bel_ascii opt =
    let f e = case e of
                Nothing -> ' '
                Just Rest -> '-'
                Just Continue -> '_'
                Just (Value c) -> c
        g = if opt then intersperse ' ' else id
    in unlines . map (g . map f) . bel_grid

-- | 'putStrLn' of 'bel_ascii'.
bel_ascii_pr :: Bel Char -> IO ()
bel_ascii_pr = putStrLn . ('\n' :) . bel_ascii True

-- * Combinators

-- | Infix form for 'Seq'.
(~>) :: Bel a -> Bel a -> Bel a
p ~> q = Seq p q

-- | 'foldl1' of 'Seq'.
--
-- > lseq [Node Rest] == Node Rest
-- > lseq [Node Rest,Node Continue] == Seq (Node Rest) (Node Continue)
lseq :: [Bel a] -> Bel a
lseq = foldl1 Seq

-- | 'Node' of 'Value'.
node :: a -> Bel a
node = Node . Value

-- | 'lseq' of 'Node'
nseq :: [a] -> Bel a
nseq = lseq . map node

-- | Variant of 'nseq' where @_@ is read as 'Continue' and @-@ as 'Rest'.
cseq :: String -> Bel Char
cseq =
    let f c = case c of
                '_' -> Continue
                '-' -> Rest
                _ -> Value c
    in foldl1 Seq . map (Node . f)

-- | 'Par' of 'Par_Max', this is the default 'Par_Mode'.
par :: Bel a -> Bel a -> Bel a
par = Par Par_Max

-- | 'Node' of 'Rest'.
rest :: Bel a
rest = Node Rest

-- | 'lseq' of 'replicate' of 'rest'.
nrests :: Integral n => n -> Bel a
nrests n = lseq (genericReplicate n rest)

-- | Verify that 'bel_char_pp' of 'bel_char_parse' is 'id'.
bel_parse_pp_ident :: String -> Bool
bel_parse_pp_ident s = bel_char_pp (bel_char_parse s) == s

-- | Run 'bel_char_parse', and print both 'bel_char_pp' and 'bel_ascii'.
--
-- > bel_ascii_pp "{i{ab,{c[d,oh]e,sr{p,qr}}},{jk,ghjkj}}"
bel_ascii_pp :: String -> IO ()
bel_ascii_pp s = do
  let p = bel_char_parse s
  putStrLn (concat ["\nBel(R): \"",bel_char_pp p,"\", Dur: ",T.rational_pp (bel_dur 1 p),""])
  bel_ascii_pr p

-- * Parsing

-- | A 'Char' parser.
type P a = P.GenParser Char () a

-- | Parse 'Rest' 'Term'.
--
-- > P.parse p_rest "" "-"
p_rest :: P (Term a)
p_rest = liftM (const Rest) (P.char '-')

-- | Parse 'Rest' 'Term'.
--
-- > P.parse p_nrests "" "3"
p_nrests :: P (Bel a)
p_nrests = liftM nrests p_integer

-- | Parse 'Continue' 'Term'.
--
-- > P.parse p_continue "" "_"
p_continue :: P (Term a)
p_continue = liftM (const Continue) (P.char '_')

-- | Parse 'Char' 'Value' 'Term'.
--
-- > P.parse p_char_value "" "a"
p_char_value :: P (Term Char)
p_char_value = liftM Value P.lower

-- | Parse 'Char' 'Term'.
--
-- > P.parse (P.many1 p_char_term) "" "-_a"
p_char_term :: P (Term Char)
p_char_term = P.choice [p_rest,p_continue,p_char_value]

-- | Parse 'Char' 'Node'.
--
-- > P.parse (P.many1 p_char_node) "" "-_a"
p_char_node :: P (Bel Char)
p_char_node = liftM Node p_char_term

-- | Parse positive 'Integer'.
--
-- > P.parse p_integer "" "3"
p_integer :: P Integer
p_integer = liftM read (P.many1 P.digit)

-- | Parse positive 'Rational'.
--
-- > P.parse (p_rational `P.sepBy` (P.char ',')) "" "3%5,2/3"
p_rational :: P Rational
p_rational = do
  n <- p_integer
  _ <- P.oneOf "%/"
  d <- p_integer
  return (n % d)

-- | Parse positive 'Double'.
--
-- > P.parse p_double "" "3.5"
-- > P.parse (p_double `P.sepBy` (P.char ',')) "" "3.5,7.2,1.0"
p_double :: P Double
p_double = do
  a <- P.many1 P.digit
  _ <- P.char '.'
  b <- P.many1 P.digit
  return (read (a ++ "." ++ b))

-- | Parse positive number as 'Rational'.
--
-- > P.parse (p_number `P.sepBy` (P.char ',')) "" "7%2,3.5,3"
p_number :: P Rational
p_number = P.choice [P.try p_rational
                    ,P.try (liftM toRational p_double)
                    ,P.try (liftM toRational p_integer)]

-- | Parse 'Mul'.
--
-- > P.parse (P.many1 p_mul) "" "/3*3/2"
p_mul :: P (Bel a)
p_mul = do
  op <- P.oneOf "*/"
  n <- p_number
  let n' = case op of
             '*' -> n
             '/' -> recip n
             _ -> error "p_mul"
  return (Mul n')

-- | Given parser for 'Bel' /a/, generate 'Iso' parser.
p_iso :: P (Bel a) -> P (Bel a)
p_iso f = do
  open <- P.oneOf "{(["
  iso <- P.many1 f
  close <- P.oneOf "})]"
  if bel_brackets_match (open,close)
    then return (Iso (lseq iso))
    else error "p_iso: open/close mismatch"

-- | 'p_iso' of 'p_char_bel'.
--
-- > P.parse p_char_iso "" "{abcde}"
p_char_iso :: P (Bel Char)
p_char_iso = p_iso p_char_bel

-- | Given parser for 'Bel' /a/, generate 'Par' parser.
p_par :: P (Bel a) -> P (Bel a)
p_par f = do
  tilde <- P.optionMaybe (P.char '~')
  open <- P.oneOf "{(["
  lhs <- P.many1 f
  _ <- P.char ','
  rhs <- P.many1 f
  close <- P.oneOf "})]"
  let m = case (tilde,open,close) of
            (Nothing,'{','}') -> Par_Max
            (Just '~','{','}') -> Par_Min
            (Nothing,'(',')') -> Par_Left
            (Just '~','(',')') -> Par_Right
            (Nothing,'[',']') -> Par_None
            _ -> error "p_par: incoherent par"
  return (Par m (lseq lhs) (lseq rhs))

-- | 'p_par' of 'p_char_bel'.
--
-- > P.parse p_char_par "" "{ab,{c,de}}"
-- > P.parse p_char_par "" "{ab,~(c,de)}"
p_char_par :: P (Bel Char)
p_char_par = p_par p_char_bel

-- | Parse 'Bel' 'Char'.
--
-- > P.parse (P.many1 p_char_bel) "" "-_a*3"
p_char_bel :: P (Bel Char)
p_char_bel = P.choice [P.try p_char_par,p_char_iso,p_mul,p_nrests,p_char_node]

-- | Run parser for 'Bel' of 'Char'.
bel_char_parse :: String -> Bel Char
bel_char_parse s =
    either
    (\e -> error ("bel_parse failed\n" ++ show e))
    lseq
    (P.parse (P.many1 p_char_bel) "" s)
