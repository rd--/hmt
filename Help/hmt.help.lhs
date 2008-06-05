> import Control.Arrow
> import Data.List
> import Data.Maybe
> import Music.Theory

$ sro T4 156
59A

> tn 4 [1,5,6]

$ sro T4I 156
3BA

> tni 4 [1,5,6]

$ echo 156 | sro T4  | sro T0I
732

> let f n = invert 0 . tn n
> in f 4 [1,5,6]

$ pcom pcseg iseg 01549 | pcom iseg icseg | pcom icseg icset
145

> (set . map ic . int) [0,1,5,4,9]

$ pcom pcseg pcset 01549 | pcom pcset sc | pcom sc icv | pcom icv icset
1345

> let icv_icset x = let f x y = if x > 0 then Just y else Nothing
>                   in catMaybes (zipWith f x [1..6])
> in (icv_icset . icv . forte_prime) [0,1,5,4,9]

Allen Forte "The Basic Interval Patterns" JMT 17/2 (1973):234-272

$ function bip { pcom pcseg iseg $ | pcom iseg icseg | nrm -r }
$ bip 0t95728e3416
11223344556
$

> bip [0,10,9,5,7,2,8,11,3,4,1,6]

$ pg 5-Z17 | bip | sort -u > 5-Z17.bip ; \
  pg 5-Z37 | bip | sort -u > 5-Z37.bip ; \
  comm 5-Z17.bip 5-Z37.bip -1 -2 | wc -l
16
$

> let f = nub . map bip . permutations . sc 
> in f "5-Z17" `intersect` f "5-Z37"

$ cat ../db.sh
for sc in $(fl -c $1)
do
  pg $sc | bip | sort -u > $sc
done
$ sh ../db.sh 4
$ ls
4-1   4-12  4-16  4-19  4-21  4-24  4-27  4-4   4-7   4-Z15
4-10  4-13  4-17  4-2   4-22  4-25  4-28  4-5   4-8   4-Z29
4-11  4-14  4-18  4-20  4-23  4-26  4-3   4-6   4-9
$

> let { s = filter ((== 4) . length) scs
>     ; x = map permutations s }
> in zip (map sc_name s) (map (set . (map bip)) x)

$ cat view.sh
for i in $(fl -c $1 | pg | bip | sort -u)
do
  echo $i":" $(grep -l $i * | sort -t '-' +1  -n | tr "\n" " ")
done
$ sh view.sh 4
111: 4-1
112: 4-1 4-2 4-3
113: 4-1 4-3 4-4 4-7
...
$

> let { n = 4
>     ; s = filter ((== n) . length) scs
>     ; x = map permutations s
>     ; z = zip (map sc_name s) (map (set . (map bip)) x)
>     ; f b (s, bs) = if b `elem` bs then Just s else Nothing
>     ; g b = catMaybes (map (f b) z) 
>     ; a = set (map bip (concat x)) }
> in zip a (map g a)

$ cyc <  ~/src/pct/lib/scs | epmq \
> "in cset 89" "is icset 12" "hasnt icseg 11" | scdb
7-34    ascending melodic minor collection
7-35    diatonic collection (d)
8-28    octotonic collection (Messiaen Mode II)
$

> let { cyc xs = xs ++ [head xs]
>     ; a = filter (\p -> length p `elem` [8,9]) (map cyc scs)
>     ; b = filter (\p -> set (int p) == [1,2]) a
>     ; c = filter (\p -> not ([1,1] `isInfixOf` int p)) b }
> in map sc_name c

$ epmq < ~/src/pct/lib/univ "in cset 6" "in pcset 579t024" \
> "has sc 5-35" "hasnt sc 2-6" "notin pcset 024579e"
02579A
$

> let { a = cf [6] (powerset [0..11])
>     ; b = filter (is_superset [0,2,4,5,7,9,10]) a
>     ; c = filter (`has_sc` (sc "5-35")) b
>     ; d = filter (not . (`has_sc` (sc "2-6"))) c }
> in filter (not . is_superset [0,2,4,5,7,9,11]) d

$ echo 024579 | sro RT4I
79B024

> sro (SRO 0 True 4 False True) [0,2,4,5,7,9]

$ sro T4I 156
3BA

> sro (SRO 0 False 4 False True) [1,5,6]

$ echo 156 | sro T0I | sro T4
3BA

> (sro (SRO 0 False 0 False True) >>> sro (SRO 0 False 4 False False)) [1,5,6]

$ echo 156 | sro T4  | sro T0I
732

> (sro (SRO 0 False 0 False True) . sro (SRO 0 False 4 False False)) [1,5,6]

$ rsg 156 3BA
T4I

> rsg [1,5,6] [3,11,10]

$ rsg 0123 05t3
T0M

> rsg [0,1,2,3] [0,5,10,3]

$ rsg 0123 4e61
RT1M

> rsg [0,1,2,3] [4,11,6,1]

$ echo e614 | rsg 0123
r3RT1M

note: pct uses right rotation rotation.

> rsg [0,1,2,3] [11,6,1,4]

> sro (SRO 1 True 1 True False) [0,1,2,3]

> sro (SRO 1 False 4 True True) [0,1,2,3]

T0 = T0M1; Tn = TnM1
I = MB; TnI = TnMB,
M = M5; TnM = TnM5,
MI = IM = M7 = MBM5; TnMI = TnM7

> mn 11 [0,1,4,9] == tni 0 [0,1,4,9]

$ se -c5 123
12333
12233
12223
11233
11223
11123
$

> se 5 [1,2,3]

$ ici -c 123
123
129
1A3
1A9
$

> ici_c [1,2,3]

> ici [1,2,3]

> cgg [[0],[1,11],[2,10],[3,9],[4,8],[5,7],[6]]

$ se -c5 1245 | pg | ici | pcom iseg sc | \
  sort -u | epmq "in cset 6" | wc -l
42
$

> let { a = se 5 [1,2,4,5]
>     ; b = concatMap permutations a
>     ; c = concatMap ici b 
>     ; d = map (forte_prime . dx_d 0) c
>     ; e = nub d 
>     ; f = cf [6] e }
> in length f

$ cg -r3 0159
015
019
059
159
$

> cg_r 3 [0,1,5,9]

$ cmpl 02468t
13579B
$

> cmpl [0,2,4,6,8,10]

$ cyc 056
0560
$

> cyc [0,5,6]

$ dim 016
T1d
T1m
T0o
$

> dim [0,1,6]

$ dis 24
1256
$

> dis [2,4]

$ echo 024579e | doi 6 | sort -u
024579A
024679B
$ echo 01234 | doi 2 7-35 | sort -u
13568AB
$

> let p = [0,2,4,5,7,9,11] in doi 6 p p

> doi 2 (sc "7-35") [0,1,2,3,4]

