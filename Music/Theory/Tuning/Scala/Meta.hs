-- | Scala DB meta-data.
module Music.Theory.Tuning.Scala.Meta where

-- | Just-intonation (ie. all rational) scales, collected by author.
scl_ji_au :: [(String,[String])]
scl_ji_au =
  [("Alves, Bill",words "alves_12 alves_22 alves_pelog alves alves_slendro")
  ,("Archytas"
   ,["arch_chrom","arch_chromc2" -- "archchro" NON-JI
    ,"arch_dor"
    ,"arch_enh","arch_enh2","arch_enh3","arch_enhp"
    ,"arch_enht","arch_enht2","arch_enht3","arch_enht4","arch_enht5","arch_enht6","arch_enht7"
    ,"arch_mult"
    ,"arch_ptol","arch_ptol2"
    ,"arch_sept"
    -- "archytas7" "archytas12","archytas12sync" NON-JI
    ])
  ,("Barlow, Clarence",words "barlow_13 barlow_17")
  ,("Boethius",words "boeth_chrom boeth_enh")
  ,("Burt, Warren",
     concat [map (\n -> "burt" ++ show n) [1::Int .. 20]
            ,words "burt_fibo burt_fibo23 burt_forks burt_primes"])
  ,("Chalmers, John"
   ,["chalmers"
    ,"chalmers_17"
    ,"chalmers_19"
    ,"chalmers_ji1"
    ,"chalmers_ji2"
    ,"chalmers_ji3"
    ,"chalmers_ji4"
    ,"corner7"
    ,"corner11"
    ,"corner13"
    ,"corners7"
    ,"corners11"
    ,"corners13"
    ,"finnamore_jc"
    ,"hamilton_jc"
    ,"major_clus"
    ,"major_wing"
    ,"minor_clus"
    ,"minor_wing"
    ,"pelog_jc"
    ,"prod7d"
    ,"prodq13"
    ,"slen_pel_jc"])
  ,("Didymus", words "didy_chrom didy_chrom1 didy_chrom2 didy_chrom3 didy_diat didy_enh didy_enh2")
  ,("Eratosthenes",words "eratos_chrom eratos_diat eratos_enh")
  ,("Euler, Leonhard",words "euler euler_diat euler_enh euler_gm")
  ,("Gann, Kyle",words "gann_arcana gann_charingcross gann_cinderella gann_custer gann_fractured gann_fugitive gann_ghost gann_love gann_new_aunts gann_revisited gann_sitting gann_solitaire gann_suntune gann_super gann_things gann_wolfe hulen_33")
  ,("Grady, Kraig"
   ,["dekany-cs"
    ,"grady11"
    ,"grady_14"
    ,"grady_centaur"
    ,"grady_centaur17"
    ,"grady_centaur19"])
  ,("Hahn, Paul",words "duohex hahn_7 hahn9 hahnmaxr indian-hahn") -- hahn_g mean14a
  ,("Harrison, Lou"
   ,["dudon_slendro_matrix" -- NON-UNIQ
    ,"harrison_5"
    ,"harrison_5_1"
    ,"harrison_5_3" -- NON-STEP
    ,"harrison_5_4" -- NON-STEP
    ,"harrison_8" -- NON-STEP
    ,"harrison_15"
    ,"harrison_16"
    ,"harrison_bill"
    ,"harrison_cinna"
    ,"harrison_diat"
    ,"harrison_handel"
    ,"harrison_kyai" -- NON-STEP
    ,"harrison_mid"
    ,"harrison_mid2"
    ,"harrison_mix2"
    ,"harrison_mix3" -- NON-STEP
    ,"harrison_mix4"
    ,"harrison_slye"
    ,"harrison_songs"
    ,"hexany10"
    ,"hirajoshi2"
    ,"korea_5"
    ,"olympos"
    ,"pelog_jc" -- STRICT SONGS
    ,"pelog_laras" -- NON-STEP
    ,"prime_5"
    ,"slendro5_1","slendro5_2"
    ,"slendro_7_1","slendro_7_2","slendro_7_3","slendro_7_4"
    -- "slendro_laras" -- NON-OCT
    ,"tranh"])
  ,("Johnston, Ben"
   ,["johnston"
    ,"johnston_21"
    ,"johnston_22"
    ,"johnston_25"
    ,"johnston_81"
    ,"johnston_6-qt"
    ,"johnston_6-qt_row"])
  ,("Kepler, Johannes",words "kepler1 kepler2 kepler3")
  ,("Partch, Harry"
   ,["kring1"
    ,"diamond7"
    ,"diamond9"
    ,"diamond17b"
    ,"novaro15"
    ,"partch_29-av"
    ,"partch_29"
    ,"partch_37"
    ,"partch_39"
    ,"partch_41"
    ,"partch_43"
    ,"partch-barstow"])
  ,("Ptolemy"
   ,["ptolemy_chrom"
    ,"ptolemy_ddiat"
    ,"ptolemy_diat","ptolemy_diat2","ptolemy_diat3","ptolemy_diat4","ptolemy_diat5"
    ,"ptolemy_diff"
    ,"ptolemy_enh"
    ,"ptolemy_exp"
    ,"ptolemy_ext"
    ,"ptolemy_hominv","ptolemy_hominv2"
    ,"ptolemy_hom"
    ,"ptolemy_iastaiol","ptolemy_iast"
    ,"ptolemy_ichrom"
    ,"ptolemy_idiat"
    ,"ptolemy_imix"
    ,"ptolemy_malak","ptolemy_malak2"
    ,"ptolemy_mdiat","ptolemy_mdiat2","ptolemy_mdiat3"
    ,"ptolemy_meta"
    ,"ptolemy_mix"
    ,"ptolemy_perm"
    ,"ptolemy_prod"
    ,"ptolemy"
    ,"ptolemy_tree"])
  ,("Pythagoras"
   ,["pyth_7a","pyth_12","pyth_12s","pyth_17","pyth_17s","pyth_22","pyth_27","pyth_chrom"
    -- "pyth_31" "pyth_sev" "pyth_third" NOT-JI
    ])
  ,("Riley, Terry",words "riley_albion riley_rosary")
  ,("Smith, Gene Ward",["smithgw_15highschool1","smithgw_15highschool2","smithgw_18","smithgw_19highschool1","smithgw_19highschool2","smithgw_21","smithgw_22highschool","smithgw_58","smithgw_9","smithgw_ball","smithgw_ball2","smithgw_circu","smithgw_decab","smithgw_decac","smithgw_decad","smithgw_diff13","smithgw_dwarf6_7","smithgw_ennon13","smithgw_ennon15","smithgw_ennon28","smithgw_ennon43","smithgw_euclid3","smithgw_glamma","smithgw_glumma","smithgw_gm","smithgw_hahn12","smithgw_hahn15","smithgw_hahn16","smithgw_hahn19","smithgw_hahn22","smithgw_indianred","smithgw_majraj1","smithgw_majraj2","smithgw_majraj3","smithgw_majsyn1","smithgw_majsyn2","smithgw_majsyn3","smithgw_meandin","smithgw_meanred","smithgw_mir22","smithgw_monzoblock37","smithgw_orw18r","smithgw_pel1","smithgw_pel3","smithgw_pris","smithgw_prisa","smithgw_ragasyn1","smithgw_ratwell","smithgw_rectoo","smithgw_red72_11geo","smithgw_red72_11pro","smithgw_sc19","smithgw_scj22a","smithgw_scj22b","smithgw_scj22c","smithgw_smalldi11","smithgw_smalldi19a","smithgw_smalldi19b","smithgw_smalldi19c","smithgw_star","smithgw_star2","smithgw_syndia2","smithgw_syndia3","smithgw_syndia4","smithgw_syndia6","smithgw_well1","smithgw_wiz28","smithgw_wiz34","smithgw_wiz38"])
  ,("Tenney, James",words "mund45 tenney_8 tenney_11 tenn41a tenn41b tenn41c")
  ,("Wilson, Erv"
   ,["chin_7"
    ,"ckring9"
    ,"diamond7-13"
    ,"dodeceny","dorian_diat2inv","hypol_diatinv"
    ,"dkring3"
    ,"efg33357","efg3335711","efg35711"
    ,"eikosany"
    ,"erlich9"
    ,"harm6","harm8","harm9","harm14","harm15"
    ,"hexany_union"
    ,"indian-magrama"
    ,"malkauns"
    ,"malcolme"
    ,"novaro15"
    ,"partch_29"
    ,"ptolemy","ptolemy_diat2","ptolemy_idiat"
    ,"slendro5_1","slendro5_2","slendro_7_4"
    ,"steldek1","steldek1s","steldek2","steldek2s"
    ,"steldia"
    ,"steleik1","steleik1s","steleik2","steleik2s"
    ,"stelhex1","stelhex2","stelhex5","stelhex6" -- stelhex3 stelhex4
    ,"stelpd1","stelpd1s"
    ,"stelpent1","stelpent1s"
    ,"steltet1","steltet1s","steltet2"
    ,"steltri1","steltri2"
    ,"tritriad14"
    ,"wilson1","wilson2","wilson3","wilson5","wilson7","wilson11"
    ,"wilson7_2","wilson7_3","wilson7_4"
    ,"wilson_17","wilson_31","wilson_41"
    ,"wilcent17"
    ,"wilson_alessandro"
    ,"wilson_bag"
    ,"wilson_class"
    ,"wilson_dia1","wilson_dia2","wilson_dia3","wilson_dia4"
    ,"wilson_duo"
    ,"wilson_enh","wilson_enh2"
    ,"wilson_facet"
    -- ,"wilson_gh1","wilson_gh2","wilson_gh11","wilson_gh50" -- NON-JI
    ,"wilson_hebdome1"
    ,"wilson_hexflank"
    ,"wilson_hypenh"
    ,"wilson-rastbayyati24"
    ,"wilson_l1","wilson_l2","wilson_l3","wilson_l4","wilson_l5","wilson_l6"])
  ,("Young, La Monte",["young-lm_guitar","young-lm_piano"])
  ]

{-
import Music.Theory.Tuning.Scala
db <- scl_load_db
nm = concatMap snd scl_ji_au
scl = filter (\x -> scale_name x `elem` nm) db
non_ji = filter (not . scl_is_ji) scl
map scale_name non_ji
-}
