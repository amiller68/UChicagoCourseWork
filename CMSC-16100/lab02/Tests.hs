module Tests where

import Data.List hiding (union)
import SimpleTime
import TimeRanges


-- For testing in GHCI

t1 = read "2011-04-05T15:45:30" :: Time
t2 = read "2012-06-02T16:36:46" :: Time
t3 = read "2013-01-04T05:36:49" :: Time
t4 = read "2014-11-18T16:03:48" :: Time

noTime  = NoTime
allTime = CTR NegEternity Eternity

t1to2 = CTR (After t1) (Before t2)
t1to3 = CTR (After t1) (Before t3)
t1to4 = CTR (After t1) (Before t4)
t2to3 = CTR (After t2) (Before t3)
t2to4 = CTR (After t2) (Before t4)
t3to4 = CTR (After t3) (Before t4)

before1 = CTR NegEternity (Before t1)
before2 = CTR NegEternity (Before t2)
before3 = CTR NegEternity (Before t3)
before4 = CTR NegEternity (Before t4)

after1 = CTR (After t1) Eternity
after2 = CTR (After t2) Eternity
after3 = CTR (After t3) Eternity
after4 = CTR (After t4) Eternity

-- Make sure read works for CTRs.
case0a = read "NoTime" :: CTR
case0b = read "<2016-05-07T03:22:53" :: CTR
case0c = read ">=2017-04-20T06:35:27" :: CTR
case0d = read "2016-05-07T03:22:53-2017-04-20T06:35:27" :: CTR
case0e = read "AllTime" :: CTR

-- Then work on the rest of the functions.

case1  = intersectCTRCTR t1to2 t2to3
case1b = intersectCTRCTR t1to2 t3to4
case2  = intersectCTRCTR t2to3 t1to2
case2b = intersectCTRCTR t3to4 t1to2
case3  = intersectCTRCTR allTime t1to2
case4  = intersectCTRCTR t1to2 allTime
case5  = intersectCTRCTR noTime t1to2
case6  = intersectCTRCTR t1to2 noTime
case7  = intersectCTRCTR t1to3 t2to4
case8  = intersectCTRCTR t2to4 t1to3
case9  = intersectCTRCTR t1to4 t2to3
case10 = intersectCTRCTR t2to3 t1to4
case11 = intersectCTRCTR t2to3 after1
case12 = intersectCTRCTR t2to4 after3
case13 = intersectCTRCTR after1 t2to3
case14 = intersectCTRCTR after3 t2to4
case15 = intersectCTRCTR t2to3 after4
case16 = intersectCTRCTR after4 t2to3
case17 = intersectCTRCTR before4 t2to3
case18 = intersectCTRCTR t2to3 before4
case19 = intersectCTRCTR before3 t2to4
case20 = intersectCTRCTR t2to4 before3
case21 = intersectCTRCTR before1 t2to4
case22 = intersectCTRCTR t2to4 before1
case23 = intersectCTRCTR before1 after1
case24 = intersectCTRCTR after1 before1
case25 = intersectCTRCTR before2 after1
case26 = intersectCTRCTR after1 before2
case27 = intersectCTRCTR after1 after2
case28 = intersectCTRCTR after2 after1
case29 = intersectCTRCTR before1 before2
case30 = intersectCTRCTR before2 before1
case31 = intersectCTRCTR before1 allTime
case32 = intersectCTRCTR allTime before1
case33 = intersectCTRCTR before1 noTime
case34 = intersectCTRCTR noTime before1
case35 = intersectCTRCTR after1 allTime
case36 = intersectCTRCTR allTime after1
case37 = intersectCTRCTR after1 noTime
case38 = intersectCTRCTR noTime after1
case39 = intersectCTRCTR noTime allTime
case40 = intersectCTRCTR allTime noTime
case41 = intersectCTRCTR noTime noTime
case42 = intersectCTRCTR allTime allTime

case43 = intersectDTRCTR [before1, t1to3, t2to4, after4] t2to3
case44 = intersectDTRCTR [before1, t1to3, t2to4, after4] after3
case45 = intersectDTRCTR [before1, t1to3, t2to4, after4] before3
case46 = intersectDTRCTR [before1, t1to3, t2to4, after4] allTime
case47 = intersectDTRCTR [before1, t1to3, t2to4, after4] noTime

case48  = differenceCTRCTR t1to2 t2to3
case48b = differenceCTRCTR t1to2 t3to4
case49  = differenceCTRCTR t2to3 t1to2
case49b = differenceCTRCTR t3to4 t1to2
case50  = differenceCTRCTR allTime t1to2
case51  = differenceCTRCTR t1to2 allTime
case52  = differenceCTRCTR noTime t1to2
case53  = differenceCTRCTR t1to2 noTime
case54  = differenceCTRCTR t1to3 t2to4
case55  = differenceCTRCTR t2to4 t1to3
case56  = differenceCTRCTR t1to4 t2to3
case57  = differenceCTRCTR t2to3 t1to4
case58  = differenceCTRCTR t2to3 after1
case59  = differenceCTRCTR t2to4 after3
case60  = differenceCTRCTR after1 t2to3
case61  = differenceCTRCTR after3 t2to4
case62  = differenceCTRCTR t2to3 after4
case63  = differenceCTRCTR after4 t2to3
case64  = differenceCTRCTR before4 t2to3
case65  = differenceCTRCTR t2to3 before4
case66  = differenceCTRCTR before3 t2to4
case67  = differenceCTRCTR t2to4 before3
case68  = differenceCTRCTR before1 t2to4
case69  = differenceCTRCTR t2to4 before1
case70  = differenceCTRCTR before1 after1
case71  = differenceCTRCTR after1 before1
case72  = differenceCTRCTR before2 after1
case73  = differenceCTRCTR after1 before2
case74  = differenceCTRCTR after1 after2
case75  = differenceCTRCTR after2 after1
case76  = differenceCTRCTR before1 before2
case77  = differenceCTRCTR before2 before1
case78  = differenceCTRCTR before1 allTime
case79  = differenceCTRCTR allTime before1
case80  = differenceCTRCTR before1 noTime
case81  = differenceCTRCTR noTime before1
case82  = differenceCTRCTR after1 allTime
case83  = differenceCTRCTR allTime after1
case84  = differenceCTRCTR after1 noTime
case85  = differenceCTRCTR noTime after1
case86  = differenceCTRCTR noTime allTime
case87  = differenceCTRCTR allTime noTime
case88  = differenceCTRCTR noTime noTime
case89  = differenceCTRCTR allTime allTime

case90 = differenceDTRCTR [before1, t1to3, t2to4, after4] t2to3
case91 = differenceDTRCTR [before1, t1to3, t2to4, after4] after3
case92 = differenceDTRCTR [before1, t1to3, t2to4, after4] before3
case93 = differenceDTRCTR [before1, t1to3, t2to4, after4] allTime
case94 = differenceDTRCTR [before1, t1to3, t2to4, after4] noTime

nicify :: DTR -> DTR
nicify = sort . nub . filter (/= NoTime)

case95  = nicify $ intersection [before1, t1to3, t2to4, after4] [before1, t1to3, t2to4, after4]
case96  = nicify $ intersection [before1, t1to3, after4] [before1, t2to4, after4]
case97  = nicify $ intersection [before1, t1to3] [t2to4, after4]
case98  = nicify $ intersection [before2, after3] [t1to3, t2to4]
case99  = nicify $ intersection [before1, t1to3, t2to4, after4] []
case100 = nicify $ intersection [] [before1, t1to3, t2to4, after4]
case101 = nicify $ intersection [before1, t1to3, t2to4, after4] [allTime]
case102 = nicify $ intersection [allTime] [before1, t1to3, t2to4, after4]

case103 = nicify $ union [before1, t1to3] [before1, t2to4, after4]

case104 = nicify $ difference [before1, t1to3, t2to4, after4] [before1, t1to3, t2to4, after4]
case105 = nicify $ difference [before1, t1to3, after4] [before1, t2to4, after4]
case106 = nicify $ difference [before1, t2to4, after4] [before1, t1to3, after4]
case107 = nicify $ difference [before1, t1to3] [t2to4, after4]
case108 = nicify $ difference [t2to4, after4] [before1, t1to3]
case109 = nicify $ difference [before2, after3] [t1to3, t2to4]
case110 = nicify $ difference [t1to3, t2to4] [before2, after3]
case111 = nicify $ difference [before1, t1to3, t2to4, after4] []
case112 = nicify $ difference [] [before1, t1to3, t2to4, after4]
case113 = nicify $ difference [before1, t1to3, t2to4, after4] [allTime]
case114 = nicify $ difference [allTime] [before1, t1to3, t2to4, after4]
case115 = nicify $ difference [before2, after2] [t1to2, t3to4, after4]
case116 = nicify $ difference [allTime] [t1to2, t3to4, after4]
case117 = nicify $ difference [allTime] [before1, after1]
case118 = nicify $ difference [allTime] [allTime]

case119 = nicify $ intersectAll [[allTime], [before1, t1to3, t2to4, after4], [before1, t1to3, t2to4, after4]]
case120 = nicify $ intersectAll [[before2, after3], [before1, t1to3, after4], [before1, t2to4, after4]]
case121 = nicify $ intersectAll [[t1to2, t2to4], [before1, t1to3], [t2to4, after4]]
case122 = nicify $ intersectAll [[allTime], [before2, after3], [t1to3, t2to4]]
case123 = nicify $ intersectAll [[before2, after3], [before1, t1to3, t2to4, after4], []]
case124 = nicify $ intersectAll [[t1to2, t2to4], [], [before1, t1to3, t2to4, after4]]
case125 = nicify $ intersectAll [[before2, after3], [before1, t1to3, t2to4, after4], [allTime]]
case126 = nicify $ intersectAll [[t1to2, t2to4], [allTime], [before1, t1to3, t2to4, after4]]

case127 = nicify $ unionAll [[allTime], [before1, t1to3], [before1, t2to4, after4]]

case128 = nicify $ differenceAll [[before1, t1to3, t2to4, after4], [before1, t1to3, t2to4, after4]]
case129 = nicify $ differenceAll [[before1, t2to4, after4], [before1, t1to3, after4]]
case130 = nicify $ differenceAll [[t2to4, after4], [before1, t1to3]]
case131 = nicify $ differenceAll [[], [before1, t1to3, t2to4, after4]]
case132 = nicify $ differenceAll [[allTime], [t1to2, t3to4, after4]]
case133 = nicify $ differenceAll [[allTime], [allTime]]
case134 = nicify $ differenceAll [[before4], [t2to3], [before1]]
case135 = nicify $ differenceAll [[before4], [t2to3, t3to4], [before1]]
case136 = nicify $ differenceAll [[allTime], [before1], [after4]]
case137 = nicify $ differenceAll [[allTime], [after1], [before4]]
case138 = nicify $ differenceAll [[allTime], [noTime], [noTime]]

case139 = areAllDisjoint [[allTime], [noTime], [noTime]] -- True
case140 = areAllDisjoint [[allTime], [noTime], [allTime]] -- False
case141 = areAllDisjoint [[before1], [t1to2], [t2to3], [t3to4], [after4]] -- True
case142 = areAllDisjoint [[before1], [t1to2], [t2to3], [t3to4], [before4]] -- False
case143 = areAllDisjoint [[t2to3], [t1to2], [t2to4]] -- False
case144 = areAllDisjoint [[t1to2], [t2to3], [t2to4]] -- False
case145 = areAllDisjoint [[t1to2, t3to4], [before1, t2to3], [after4]] -- True

case146 = areAllEqual [[allTime], [allTime, allTime], [noTime]] -- False
case147 = areAllEqual [[allTime], [allTime, allTime], [allTime]] -- True
case148 = areAllEqual [[allTime], [before1, after1], [before2, t2to3, after3]] -- True
case149 = areAllEqual [[t1to4], [t2to4, t1to2], [t1to2, t2to3, t3to4]] -- True
case150 = areAllEqual [[t1to4], [t2to4, t1to2], [t1to2, t2to3]] -- False
case151 = areAllEqual [[before4], [before3, t2to4], [before1, before2, t2to4]] -- True
case152 = areAllEqual [[after1], [after2], [after1, after2]] -- False
case153 = areAllEqual [[t1to2, t2to3], [t1to2, t2to3], [t1to2, t2to3, t3to4]]
