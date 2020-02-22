-- | Scala DB meta-data.
module Music.Theory.Tuning.Scala.Meta where

-- | Just-intonation (ie. all rational) scales, collected by author.
scl_ji_au :: [(String,[String])]
scl_ji_au =
  [("Alves, Bill",words "alves_12 alves_22 alves_pelog alves alves_slendro")
  ,("Barlow, Clarence",words "barlow_13 barlow_17")
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
   ,("Euler, Leonhard",words "euler euler_diat euler_enh euler_gm")
   ,("Gann, Kyle",words "gann_arcana gann_charingcross gann_cinderella gann_custer gann_fractured gann_fugitive gann_ghost gann_love gann_new_aunts gann_revisited gann_sitting gann_solitaire gann_suntune gann_super gann_things gann_wolfe hulen_33")
   ,("Grady, Kraig"
   ,["dekany-cs"
    ,"grady11"
    ,"grady_14"
    ,"grady_centaur"
    ,"grady_centaur17"
    ,"grady_centaur19"])
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
    ,"korea_5"
    ,"pelog_laras" -- NON-STEP
    ,"prime_5"
    ,"slendro5_1"
    ,"slendro_7_1"
    ,"slendro_7_2"
    ,"slendro_7_3"
    ,"slendro_7_4"]) -- ("slendro_laras" -- NON-OCT
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
  ,("Tenney, James",words "mund45 tenney_8 tenney_11 tenn41a tenn41b tenn41c")
  ,("Wilson, Erv"
   ,["chin_7"
    ,"ckring9"
    ,"diamond7-13"
    ,"hexany_union"
    ,"novaro15"
    ,"partch_29"
    ,"ptolemy_diat2","ptolemy_idiat"
    ,"slendro5_2"
    ,"stelhex1","stelhex2","stelhex5","stelhex6" -- stelhex3 stelhex4
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
