-- | Constants names for 'Pitch' values.  /eses/ indicates double
-- flat, /eseh/ three quarter tone flat, /es/ flat, /eh/ quarter tone
-- flat, /ih/ quarter tone sharp, /is/ sharp, /isih/ three quarter
-- tone sharp and /isis/ double sharp.
module Music.Theory.Pitch.Name where

import Music.Theory.Pitch

a0,b0 :: Pitch
a0 = Pitch A Natural 0
b0 = Pitch B Natural 0

bes0 :: Pitch
bes0 = Pitch B Flat 0

ais0,bis0 :: Pitch
ais0 = Pitch A Sharp 0
bis0 = Pitch B Sharp 0

c1,d1,e1,f1,g1,a1,b1 :: Pitch
c1 = Pitch C Natural 1
d1 = Pitch D Natural 1
e1 = Pitch E Natural 1
f1 = Pitch F Natural 1
g1 = Pitch G Natural 1
a1 = Pitch A Natural 1
b1 = Pitch B Natural 1

ces1,des1,ees1,fes1,ges1,aes1,bes1 :: Pitch
ces1 = Pitch C Flat 1
des1 = Pitch D Flat 1
ees1 = Pitch E Flat 1
fes1 = Pitch F Flat 1
ges1 = Pitch G Flat 1
aes1 = Pitch A Flat 1
bes1 = Pitch B Flat 1

cis1,dis1,eis1,fis1,gis1,ais1,bis1 :: Pitch
cis1 = Pitch C Sharp 1
dis1 = Pitch D Sharp 1
eis1 = Pitch E Sharp 1
fis1 = Pitch F Sharp 1
gis1 = Pitch G Sharp 1
ais1 = Pitch A Sharp 1
bis1 = Pitch B Sharp 1

c2,d2,e2,f2,g2,a2,b2 :: Pitch
c2 = Pitch C Natural 2
d2 = Pitch D Natural 2
e2 = Pitch E Natural 2
f2 = Pitch F Natural 2
g2 = Pitch G Natural 2
a2 = Pitch A Natural 2
b2 = Pitch B Natural 2

ces2,des2,ees2,fes2,ges2,aes2,bes2 :: Pitch
ces2 = Pitch C Flat 2
des2 = Pitch D Flat 2
ees2 = Pitch E Flat 2
fes2 = Pitch F Flat 2
ges2 = Pitch G Flat 2
aes2 = Pitch A Flat 2
bes2 = Pitch B Flat 2

cis2,dis2,eis2,fis2,gis2,ais2,bis2 :: Pitch
cis2 = Pitch C Sharp 2
dis2 = Pitch D Sharp 2
eis2 = Pitch E Sharp 2
fis2 = Pitch F Sharp 2
gis2 = Pitch G Sharp 2
ais2 = Pitch A Sharp 2
bis2 = Pitch B Sharp 2

cisis2,disis2,eisis2,fisis2,gisis2,aisis2,bisis2 :: Pitch
cisis2 = Pitch C DoubleSharp 2
disis2 = Pitch D DoubleSharp 2
eisis2 = Pitch E DoubleSharp 2
fisis2 = Pitch F DoubleSharp 2
gisis2 = Pitch G DoubleSharp 2
aisis2 = Pitch A DoubleSharp 2
bisis2 = Pitch B DoubleSharp 2

c3,d3,e3,f3,g3,a3,b3 :: Pitch
c3 = Pitch C Natural 3
d3 = Pitch D Natural 3
e3 = Pitch E Natural 3
f3 = Pitch F Natural 3
g3 = Pitch G Natural 3
a3 = Pitch A Natural 3
b3 = Pitch B Natural 3

ces3,des3,ees3,fes3,ges3,aes3,bes3 :: Pitch
ces3 = Pitch C Flat 3
des3 = Pitch D Flat 3
ees3 = Pitch E Flat 3
fes3 = Pitch F Flat 3
ges3 = Pitch G Flat 3
aes3 = Pitch A Flat 3
bes3 = Pitch B Flat 3

cis3,dis3,eis3,fis3,gis3,ais3,bis3 :: Pitch
cis3 = Pitch C Sharp 3
dis3 = Pitch D Sharp 3
eis3 = Pitch E Sharp 3
fis3 = Pitch F Sharp 3
gis3 = Pitch G Sharp 3
ais3 = Pitch A Sharp 3
bis3 = Pitch B Sharp 3

cisis3,disis3,eisis3,fisis3,gisis3,aisis3,bisis3 :: Pitch
cisis3 = Pitch C DoubleSharp 3
disis3 = Pitch D DoubleSharp 3
eisis3 = Pitch E DoubleSharp 3
fisis3 = Pitch F DoubleSharp 3
gisis3 = Pitch G DoubleSharp 3
aisis3 = Pitch A DoubleSharp 3
bisis3 = Pitch B DoubleSharp 3

ceseh3,deseh3,eeseh3,feseh3,geseh3,aeseh3,beseh3 :: Pitch
ceseh3 = Pitch C ThreeQuarterToneFlat 3
deseh3 = Pitch D ThreeQuarterToneFlat 3
eeseh3 = Pitch E ThreeQuarterToneFlat 3
feseh3 = Pitch F ThreeQuarterToneFlat 3
geseh3 = Pitch G ThreeQuarterToneFlat 3
aeseh3 = Pitch A ThreeQuarterToneFlat 3
beseh3 = Pitch B ThreeQuarterToneFlat 3

ceh3,deh3,eeh3,feh3,geh3,aeh3,beh3 :: Pitch
ceh3 = Pitch C QuarterToneFlat 3
deh3 = Pitch D QuarterToneFlat 3
eeh3 = Pitch E QuarterToneFlat 3
feh3 = Pitch F QuarterToneFlat 3
geh3 = Pitch G QuarterToneFlat 3
aeh3 = Pitch A QuarterToneFlat 3
beh3 = Pitch B QuarterToneFlat 3

cih3,dih3,eih3,fih3,gih3,aih3,bih3 :: Pitch
cih3 = Pitch C QuarterToneSharp 3
dih3 = Pitch D QuarterToneSharp 3
eih3 = Pitch E QuarterToneSharp 3
fih3 = Pitch F QuarterToneSharp 3
gih3 = Pitch G QuarterToneSharp 3
aih3 = Pitch A QuarterToneSharp 3
bih3 = Pitch B QuarterToneSharp 3

cisih3,disih3,eisih3,fisih3,gisih3,aisih3,bisih3 :: Pitch
cisih3 = Pitch C ThreeQuarterToneSharp 3
disih3 = Pitch D ThreeQuarterToneSharp 3
eisih3 = Pitch E ThreeQuarterToneSharp 3
fisih3 = Pitch F ThreeQuarterToneSharp 3
gisih3 = Pitch G ThreeQuarterToneSharp 3
aisih3 = Pitch A ThreeQuarterToneSharp 3
bisih3 = Pitch B ThreeQuarterToneSharp 3

c4,d4,e4,f4,g4,a4,b4 :: Pitch
c4 = Pitch C Natural 4
d4 = Pitch D Natural 4
e4 = Pitch E Natural 4
f4 = Pitch F Natural 4
g4 = Pitch G Natural 4
a4 = Pitch A Natural 4
b4 = Pitch B Natural 4

ces4,des4,ees4,fes4,ges4,aes4,bes4 :: Pitch
ces4 = Pitch C Flat 4
des4 = Pitch D Flat 4
ees4 = Pitch E Flat 4
fes4 = Pitch F Flat 4
ges4 = Pitch G Flat 4
aes4 = Pitch A Flat 4
bes4 = Pitch B Flat 4

cis4,dis4,eis4,fis4,gis4,ais4,bis4 :: Pitch
cis4 = Pitch C Sharp 4
dis4 = Pitch D Sharp 4
eis4 = Pitch E Sharp 4
fis4 = Pitch F Sharp 4
gis4 = Pitch G Sharp 4
ais4 = Pitch A Sharp 4
bis4 = Pitch B Sharp 4

ceses4,deses4,eeses4,feses4,geses4,aeses4,beses4 :: Pitch
ceses4 = Pitch C DoubleFlat 4
deses4 = Pitch D DoubleFlat 4
eeses4 = Pitch E DoubleFlat 4
feses4 = Pitch F DoubleFlat 4
geses4 = Pitch G DoubleFlat 4
aeses4 = Pitch A DoubleFlat 4
beses4 = Pitch B DoubleFlat 4

cisis4,disis4,eisis4,fisis4,gisis4,aisis4,bisis4 :: Pitch
cisis4 = Pitch C DoubleSharp 4
disis4 = Pitch D DoubleSharp 4
eisis4 = Pitch E DoubleSharp 4
fisis4 = Pitch F DoubleSharp 4
gisis4 = Pitch G DoubleSharp 4
aisis4 = Pitch A DoubleSharp 4
bisis4 = Pitch B DoubleSharp 4

ceseh4,deseh4,eeseh4,feseh4,geseh4,aeseh4,beseh4 :: Pitch
ceseh4 = Pitch C ThreeQuarterToneFlat 4
deseh4 = Pitch D ThreeQuarterToneFlat 4
eeseh4 = Pitch E ThreeQuarterToneFlat 4
feseh4 = Pitch F ThreeQuarterToneFlat 4
geseh4 = Pitch G ThreeQuarterToneFlat 4
aeseh4 = Pitch A ThreeQuarterToneFlat 4
beseh4 = Pitch B ThreeQuarterToneFlat 4

ceh4,deh4,eeh4,feh4,geh4,aeh4,beh4 :: Pitch
ceh4 = Pitch C QuarterToneFlat 4
deh4 = Pitch D QuarterToneFlat 4
eeh4 = Pitch E QuarterToneFlat 4
feh4 = Pitch F QuarterToneFlat 4
geh4 = Pitch G QuarterToneFlat 4
aeh4 = Pitch A QuarterToneFlat 4
beh4 = Pitch B QuarterToneFlat 4

cih4,dih4,eih4,fih4,gih4,aih4,bih4 :: Pitch
cih4 = Pitch C QuarterToneSharp 4
dih4 = Pitch D QuarterToneSharp 4
eih4 = Pitch E QuarterToneSharp 4
fih4 = Pitch F QuarterToneSharp 4
gih4 = Pitch G QuarterToneSharp 4
aih4 = Pitch A QuarterToneSharp 4
bih4 = Pitch B QuarterToneSharp 4

cisih4,disih4,eisih4,fisih4,gisih4,aisih4,bisih4 :: Pitch
cisih4 = Pitch C ThreeQuarterToneSharp 4
disih4 = Pitch D ThreeQuarterToneSharp 4
eisih4 = Pitch E ThreeQuarterToneSharp 4
fisih4 = Pitch F ThreeQuarterToneSharp 4
gisih4 = Pitch G ThreeQuarterToneSharp 4
aisih4 = Pitch A ThreeQuarterToneSharp 4
bisih4 = Pitch B ThreeQuarterToneSharp 4

c5,d5,e5,f5,g5,a5,b5 :: Pitch
c5 = Pitch C Natural 5
d5 = Pitch D Natural 5
e5 = Pitch E Natural 5
f5 = Pitch F Natural 5
g5 = Pitch G Natural 5
a5 = Pitch A Natural 5
b5 = Pitch B Natural 5

ces5,des5,ees5,fes5,ges5,aes5,bes5 :: Pitch
ces5 = Pitch C Flat 5
des5 = Pitch D Flat 5
ees5 = Pitch E Flat 5
fes5 = Pitch F Flat 5
ges5 = Pitch G Flat 5
aes5 = Pitch A Flat 5
bes5 = Pitch B Flat 5

cis5,dis5,eis5,fis5,gis5,ais5,bis5 :: Pitch
cis5 = Pitch C Sharp 5
dis5 = Pitch D Sharp 5
eis5 = Pitch E Sharp 5
fis5 = Pitch F Sharp 5
gis5 = Pitch G Sharp 5
ais5 = Pitch A Sharp 5
bis5 = Pitch B Sharp 5

ceses5,deses5,eeses5,feses5,geses5,aeses5,beses5 :: Pitch
ceses5 = Pitch C DoubleFlat 5
deses5 = Pitch D DoubleFlat 5
eeses5 = Pitch E DoubleFlat 5
feses5 = Pitch F DoubleFlat 5
geses5 = Pitch G DoubleFlat 5
aeses5 = Pitch A DoubleFlat 5
beses5 = Pitch B DoubleFlat 5

cisis5,disis5,eisis5,fisis5,gisis5,aisis5,bisis5 :: Pitch
cisis5 = Pitch C DoubleSharp 5
disis5 = Pitch D DoubleSharp 5
eisis5 = Pitch E DoubleSharp 5
fisis5 = Pitch F DoubleSharp 5
gisis5 = Pitch G DoubleSharp 5
aisis5 = Pitch A DoubleSharp 5
bisis5 = Pitch B DoubleSharp 5

ceseh5,deseh5,eeseh5,feseh5,geseh5,aeseh5,beseh5 :: Pitch
ceseh5 = Pitch C ThreeQuarterToneFlat 5
deseh5 = Pitch D ThreeQuarterToneFlat 5
eeseh5 = Pitch E ThreeQuarterToneFlat 5
feseh5 = Pitch F ThreeQuarterToneFlat 5
geseh5 = Pitch G ThreeQuarterToneFlat 5
aeseh5 = Pitch A ThreeQuarterToneFlat 5
beseh5 = Pitch B ThreeQuarterToneFlat 5

ceh5,deh5,eeh5,feh5,geh5,aeh5,beh5 :: Pitch
ceh5 = Pitch C QuarterToneFlat 5
deh5 = Pitch D QuarterToneFlat 5
eeh5 = Pitch E QuarterToneFlat 5
feh5 = Pitch F QuarterToneFlat 5
geh5 = Pitch G QuarterToneFlat 5
aeh5 = Pitch A QuarterToneFlat 5
beh5 = Pitch B QuarterToneFlat 5

cih5,dih5,eih5,fih5,gih5,aih5,bih5 :: Pitch
cih5 = Pitch C QuarterToneSharp 5
dih5 = Pitch D QuarterToneSharp 5
eih5 = Pitch E QuarterToneSharp 5
fih5 = Pitch F QuarterToneSharp 5
gih5 = Pitch G QuarterToneSharp 5
aih5 = Pitch A QuarterToneSharp 5
bih5 = Pitch B QuarterToneSharp 5

cisih5,disih5,eisih5,fisih5,gisih5,aisih5,bisih5 :: Pitch
cisih5 = Pitch C ThreeQuarterToneSharp 5
disih5 = Pitch D ThreeQuarterToneSharp 5
eisih5 = Pitch E ThreeQuarterToneSharp 5
fisih5 = Pitch F ThreeQuarterToneSharp 5
gisih5 = Pitch G ThreeQuarterToneSharp 5
aisih5 = Pitch A ThreeQuarterToneSharp 5
bisih5 = Pitch B ThreeQuarterToneSharp 5

c6,d6,e6,f6,g6,a6,b6 :: Pitch
c6 = Pitch C Natural 6
d6 = Pitch D Natural 6
e6 = Pitch E Natural 6
f6 = Pitch F Natural 6
g6 = Pitch G Natural 6
a6 = Pitch A Natural 6
b6 = Pitch B Natural 6

ces6,des6,ees6,fes6,ges6,aes6,bes6 :: Pitch
ces6 = Pitch C Flat 6
des6 = Pitch D Flat 6
ees6 = Pitch E Flat 6
fes6 = Pitch F Flat 6
ges6 = Pitch G Flat 6
aes6 = Pitch A Flat 6
bes6 = Pitch B Flat 6

cis6,dis6,eis6,fis6,gis6,ais6,bis6 :: Pitch
cis6 = Pitch C Sharp 6
dis6 = Pitch D Sharp 6
eis6 = Pitch E Sharp 6
fis6 = Pitch F Sharp 6
gis6 = Pitch G Sharp 6
ais6 = Pitch A Sharp 6
bis6 = Pitch B Sharp 6

ceseh6,deseh6,eeseh6,feseh6,geseh6,aeseh6,beseh6 :: Pitch
ceseh6 = Pitch C ThreeQuarterToneFlat 6
deseh6 = Pitch D ThreeQuarterToneFlat 6
eeseh6 = Pitch E ThreeQuarterToneFlat 6
feseh6 = Pitch F ThreeQuarterToneFlat 6
geseh6 = Pitch G ThreeQuarterToneFlat 6
aeseh6 = Pitch A ThreeQuarterToneFlat 6
beseh6 = Pitch B ThreeQuarterToneFlat 6

ceh6,deh6,eeh6,feh6,geh6,aeh6,beh6 :: Pitch
ceh6 = Pitch C QuarterToneFlat 6
deh6 = Pitch D QuarterToneFlat 6
eeh6 = Pitch E QuarterToneFlat 6
feh6 = Pitch F QuarterToneFlat 6
geh6 = Pitch G QuarterToneFlat 6
aeh6 = Pitch A QuarterToneFlat 6
beh6 = Pitch B QuarterToneFlat 6

cih6,dih6,eih6,fih6,gih6,aih6,bih6 :: Pitch
cih6 = Pitch C QuarterToneSharp 6
dih6 = Pitch D QuarterToneSharp 6
eih6 = Pitch E QuarterToneSharp 6
fih6 = Pitch F QuarterToneSharp 6
gih6 = Pitch G QuarterToneSharp 6
aih6 = Pitch A QuarterToneSharp 6
bih6 = Pitch B QuarterToneSharp 6

cisih6,disih6,eisih6,fisih6,gisih6,aisih6,bisih6 :: Pitch
cisih6 = Pitch C ThreeQuarterToneSharp 6
disih6 = Pitch D ThreeQuarterToneSharp 6
eisih6 = Pitch E ThreeQuarterToneSharp 6
fisih6 = Pitch F ThreeQuarterToneSharp 6
gisih6 = Pitch G ThreeQuarterToneSharp 6
aisih6 = Pitch A ThreeQuarterToneSharp 6
bisih6 = Pitch B ThreeQuarterToneSharp 6

c7,d7,e7,f7,g7,a7,b7 :: Pitch
c7 = Pitch C Natural 7
d7 = Pitch D Natural 7
e7 = Pitch E Natural 7
f7 = Pitch F Natural 7
g7 = Pitch G Natural 7
a7 = Pitch A Natural 7
b7 = Pitch B Natural 7

ces7,des7,ees7,fes7,ges7,aes7,bes7 :: Pitch
ces7 = Pitch C Flat 7
des7 = Pitch D Flat 7
ees7 = Pitch E Flat 7
fes7 = Pitch F Flat 7
ges7 = Pitch G Flat 7
aes7 = Pitch A Flat 7
bes7 = Pitch B Flat 7

cis7,dis7,eis7,fis7,gis7,ais7,bis7 :: Pitch
cis7 = Pitch C Sharp 7
dis7 = Pitch D Sharp 7
eis7 = Pitch E Sharp 7
fis7 = Pitch F Sharp 7
gis7 = Pitch G Sharp 7
ais7 = Pitch A Sharp 7
bis7 = Pitch B Sharp 7

c8,cis8,d8 :: Pitch
c8 = Pitch C Natural 8
cis8 = Pitch C Sharp 8
d8 = Pitch D Natural 8
