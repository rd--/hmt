-- | DB of locally defined tunings, but for ordinary use see "Music.Theory.Tuning.Scala".
module Music.Theory.Tuning.DB where

import Music.Theory.Tuning
import Music.Theory.Tuning.Alves
import Music.Theory.Tuning.Alves_1997
import Music.Theory.Tuning.Gann
import Music.Theory.Tuning.Microtonal_Synthesis

type Named_Tuning = (String,String,String,String,Tuning,String)

tuning_db :: [Named_Tuning]
tuning_db =
    [("Aaron","Pietro","","1523",pietro_aaron_1523,"meanquar")
    ,("Alves","Bill","Slendro","",alves_slendro,"slendro_alves")
    ,("Alves","Bill","Pelog/Bem","",alves_pelog_bem,"")
    ,("Alves","Bill","Pelog/Barang","",alves_pelog_barang,"")
    ,("Harrison","Lou","Ditone","",harrison_ditone,"")
    ,("Harrison","Lou","16-tone","",lou_harrison_16,"harrison_16")
    ,("Johnston","Ben","MTP","1977",ben_johnston_mtp_1977,"")
    ,("Johnston","Ben","25-tone","",ben_johnston_25,"johnston_25")
    ,("Kirnberger","Johann Philipp","III","",kirnberger_iii,"kirnberger")
    ,("Partch","Harry","43-tone","",partch_43,"partch_43")
    ,("Tsuda","Mayumi","13-limit","",mayumi_tsuda,"tsuda13")
    ,("Vallotti","","","1754",vallotti,"vallotti")
    ,("Young","La Monte","The Well-Tuned Piano","",la_monte_young_wtp,"young-lm_piano")
    ,("Young","Thomas","","1799",thomas_young_1799,"young2")
    ,("Zarlino","Gioseffo","","1588",zarlino_1588,"zarlino2")
    ,("","","ET/12","",equal_temperament_12,"")
    ,("","","ET/19","",equal_temperament_19,"")
    ,("","","ET/31","",equal_temperament_31,"")
    ,("","","ET/53","",equal_temperament_53,"")
    ,("","","ET/72","",equal_temperament_72,"")
    ,("","","ET/96","",equal_temperament_96,"")
    ]
