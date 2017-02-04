-- | DB of locally defined tunings, but for ordinary use see "Music.Theory.Tuning.Scala".
module Music.Theory.Tuning.DB where

import Data.List {- base -}

import Music.Theory.Tuning

import Music.Theory.Tuning.Alves_1997
import Music.Theory.Tuning.Polansky_1978
import Music.Theory.Tuning.Polansky_1985c

import Music.Theory.Tuning.DB.Alves
import Music.Theory.Tuning.DB.Gann
import Music.Theory.Tuning.DB.Microtonal_Synthesis
import Music.Theory.Tuning.DB.Riley
import Music.Theory.Tuning.DB.Werckmeister

-- | (last-name,first-name,title,year,hmt/tuning,scala/name)
type Named_Tuning = (String,String,String,String,Tuning,String)

named_tuning_t :: Named_Tuning -> Tuning
named_tuning_t (_,_,_,_,t,_) = t

tuning_db :: [Named_Tuning]
tuning_db =
    [("Aaron","Pietro","","1523",pietro_aaron_1523,"meanquar")
    ,("Alves","Bill","Slendro","",alves_slendro,"slendro_alves")
    ,("Alves","Bill","Pelog/Bem","",alves_pelog_bem,"")
    ,("Alves","Bill","Pelog/Barang","",alves_pelog_barang,"")
    ,("Gann","Kyle","Superparticular","1992",gann_superparticular,"gann_super")
    ,("Harrison","Lou","Ditone","",harrison_ditone,"")
    ,("Harrison","Lou","16-tone","",lou_harrison_16,"harrison_16")
    ,("Johnston","Ben","MTP","1977",ben_johnston_mtp_1977,"")
    ,("Johnston","Ben","25-tone","",ben_johnston_25,"johnston_25")
    ,("Kirnberger","Johann Philipp","III","",kirnberger_iii,"kirnberger")
    ,("Malcolm","Alexander","Monochord","1721",five_limit_tuning,"malcolm")
    ,("Partch","Harry","43-tone","",partch_43,"partch_43")
    ,("Polansky","Larry","Piano Study #5","1985",ps5_jpr,"polansky_ps")
    ,("Polansky","Larry","Psaltery","1978",psaltery_o,"")
    ,("Riley","Terry","Harp of New Albion","",riley_albion,"riley_albion")
    ,("Tsuda","Mayumi","13-limit","",mayumi_tsuda,"tsuda13")
    ,("Vallotti","","","1754",vallotti,"vallotti")
    ,("Werckmeister","Andreas","Werckmeister III","",werckmeister_iii,"werck3")
    ,("Werckmeister","Andreas","Werckmeister IV","",werckmeister_iv,"werck4")
    ,("Werckmeister","Andreas","Werckmeister V","",werckmeister_v,"werck5")
    ,("Werckmeister","Andreas","Werckmeister VI","",werckmeister_vi,"werck6")
    ,("Young","La Monte","The Well-Tuned Piano","",la_monte_young_wtp,"young-lm_piano")
    ,("Young","Thomas","","1799",thomas_young_1799,"young2")
    ,("Zarlino","Gioseffo","","1588",zarlino_1588,"zarlino2")
    ,("","","JI/12 7-limit","",septimal_tritone_just_intonation,"ji_12")
    ,("","","ET/12","",equal_temperament_12,"")
    ,("","","ET/19","",equal_temperament_19,"")
    ,("","","ET/31","",equal_temperament_31,"")
    ,("","","ET/53","",equal_temperament_53,"")
    ,("","","ET/72","",equal_temperament_72,"")
    ,("","","ET/96","",equal_temperament_96,"")
    ,("","","Pythagorean/12","",pythagorean_12,"pyth_12")
    ]

tuning_db_lookup_scl :: String -> Maybe Tuning
tuning_db_lookup_scl nm = fmap named_tuning_t (find (\(_,_,_,_,_,scl) -> scl == nm) tuning_db)
