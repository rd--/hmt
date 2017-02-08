  Pct

> import Control.Arrow {- base -}
> import Data.Function {- base -}
> import Data.List {- base -}
> import Data.Maybe {- base -}

> import Music.Theory.List {- hmt -}
> import Music.Theory.Permutations {- hmt -}
> import Music.Theory.Set.List {- hmt -}
> import Music.Theory.Z.SRO {- hmt -}
> import Music.Theory.Z12.Drape_1999 {- hmt -}
> import Music.Theory.Z12.Forte_1973 {- hmt -}
> import Music.Theory.Z12.Morris_1987 {- hmt -}
> import Music.Theory.Z12.Morris_1987.Parse {- hmt -}
> import Music.Theory.Z12.SRO {- hmt -}

This file illustrates equivalent expressions in pct and hmt terms.

    $ pcom pcseg iseg 01549 | pcom iseg icseg | pcom icseg icset
    145

> x_00 = (set . map ic . int) [0,1,5,4,9] == [1,4,5]

    $ pcom pcseg pcset 01549 | pcom pcset sc | pcom sc icv | pcom icv icset
    1345

> icv_icset x =
>     let f x y = if x > 0 then Just y else Nothing
>     in catMaybes (zipWith f x [1..6])

> x_01 = (icv_icset . icv . forte_prime) [0,1,5,4,9] == [1,3,4,5]

    $ pg 5-Z17 | bip | sort -u > 5-Z17.bip ; \
      pg 5-Z37 | bip | sort -u > 5-Z37.bip ; \
      comm 5-Z17.bip 5-Z37.bip -1 -2 | wc -l
    16

> x_02 =
>     let f = nub . map bip . permutations . sc
>     in length (f "5-Z17" `intersect` f "5-Z37") == 16

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

> x_03 =
>     let {s = filter ((== 4) . length) scs
>         ;x = map permutations s}
>     in zip (map sc_name s) (map (set . (map bip)) x)

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

> x_04 =
>     let {n = 4
>         ;s = filter ((== n) . length) scs
>         ;x = map permutations s
>         ;z = zip (map sc_name s) (map (set . (map bip)) x)
>         ;f b (s,bs) = if b `elem` bs then Just s else Nothing
>         ;g b = catMaybes (map (f b) z)
>         ;a = set (map bip (concat x))}
>     in zip a (map g a)

    $ cyc <  ~/src/pct/lib/scs | epmq \
    > "in cset 89" "is icset 12" "hasnt icseg 11" | scdb
    7-34    ascending melodic minor collection
    7-35    diatonic collection (d)
    8-28    octotonic collection (Messiaen Mode II)

> x_05 =
>     let {cyc xs = xs ++ [head xs]
>         ;a = filter (\p -> length p `elem` [8,9]) (map cyc scs)
>         ;b = filter (\p -> set (int p) == [1,2]) a
>         ;c = filter (\p -> not ([1,1] `isInfixOf` int p)) b}
>     in map (sc_name . nub) c == ["7-34","7-35","8-28"]

    $ epmq < ~/src/pct/lib/univ "in cset 6" "in pcset 579t024" \
    > "has sc 5-35" "hasnt sc 2-6" "notin pcset 024579e"
    02579A

> x_06 =
>     let {a = cf [6] (powerset [0..11])
>         ;b = filter (is_superset [0,2,4,5,7,9,10]) a
>         ;c = filter (`has_sc` (sc "5-35")) b
>         ;d = filter (not . (`has_sc` (sc "2-6"))) c
>         ;e = filter (not . is_superset [0,2,4,5,7,9,11]) d}
>     in e == [[0,2,5,7,9,10]]

    $ echo 156 | sro T0I | sro T4
    3BA

> x_07 =
>     let {i = SRO 0 False 0 False True
>         ;t4 = SRO 0 False 4 False False}
>     in (sro i >>> sro t4) [1,5,6] == [3,11,10]

    $ echo 156 | sro T4  | sro T0I
    732

> x_08 =
>     let {i = SRO 0 False 0 False True
>         ;t4 = SRO 0 False 4 False False}
>     in (sro i . sro t4) [1,5,6] == [7,3,2]

Note that pct uses right rotation rotation.

> x_09 = sro (SRO 1 True 1 True False) [0,1,2,3] == [11,6,1,4]

> x_10 = sro (SRO 1 False 4 True True) [0,1,2,3] == [11,6,1,4]

    I = MB; TnI = TnMB,

> x_11 = sro_mn 11 [0,1,4,9] == sro_tni 0 [0,1,4,9]

    MI = IM = M7 = MBM5; TnMI = TnM7

> x_12 = sro (sro_parse "T0MI") [0,1,4,9] == sro_mn 7 [0,1,4,9]

    T0 = T0M1; Tn = TnM1
    M = M5; TnM = TnM5,

    $ se -c5 123
    12333
    12233
    12223
    11233
    11223
    11123

> x_13 =
>     let r = [[1,1,1,2,3],[1,1,2,2,3],[1,1,2,3,3]
>             ,[1,2,2,2,3],[1,2,2,3,3],[1,2,3,3,3]]
>     in expand_set 5 [1,2,3] == r

> x_14 =
>     let r = [[1,2,3],[1,2,9],[1,10,3],[1,10,9]
>             ,[11,2,3],[11,2,9],[11,10,3],[11,10,9]]
>     in ici [1,2,3]

> x_15 = cgg [[0],[1,11],[2,10],[3,9],[4,8],[5,7],[6]]

    $ pct se -c5 1245 | pct pg | pct ici | pct pcom iseg sc | \
      sort -u | pct epmq "in cset 6" | wc -l
    42

> x_16 =
>     let {a = expand_set 5 [1,2,4,5]
>         ;b = concatMap permutations a
>         ;c = concatMap ici b
>         ;d = map (forte_prime . dx_d 0) c
>         ;e = nub d
>         ;f = cf [6] e}
>     in length f == 42

    $ imb -c34 024579 | pfmt
    024 245 457 579
    0245 2457 4579

> x_17 =
>     let r = [[[0,2,4],[2,4,5],[4,5,7],[5,7,9]]
>             ,[[0,2,4,5],[2,4,5,7],[4,5,7,9]]]
>     in imb [3,4] [0,2,4,5,7,9]

    $ rs 0123 e614
    T1M
    $ rs 0123 641e416
    T1M

    $ sb 6-32 6-8 | fn | pfmt
    1-1
    2-1 2-2 2-3 2-4 2-5
    3-2 3-4 3-6 3-7 3-9 3-11
    4-10 4-11 4-14 4-22 4-23
    5-23
    $ for i in `cat ~/src/pct/lib/scs | cf 6 | fn` ; \
      do echo $i >> LIST ; sb $i | cf 3 | wc -l >> LIST ; done

> x_18 =
>     let r = ["0-1"
>             ,"1-1"
>             ,"2-1","2-2","2-3","2-4","2-5"
>             ,"3-2","3-4","3-6","3-7","3-9","3-11"
>             ,"4-10","4-11","4-14","4-22","4-23"
>             ,"5-23"]
>     in map sc_name (sb [sc "6-32",sc "6-8"])

> x_19 =
>     let f p = let xs = cf [3] (sb [p])
>               in (sc_name p,length xs)
>     in map f (cf [6] scs)
