SELECT T."ID", T."ID_TYPE", T."Profile", T."X", T."Y", T."Z", T."AREA", T."DENSITY", T."OreType", T."OreBody", T."OreBlock", T."Pb", T."Pbf", T."Zn", T."Znf", T."BaSO4", T."Cu", T."Pb_03", T."Zn_06", T."BaSO4_6", T."Metall_Zn", T."Metall_Ba", T."VARIANT3_Zn", T."VARIANT2_Zn", T."VARIANT1_Zn", T."VARIANT1_Ba", T."VARIANT2_Ba", T."VARIANT3_Ba", T."NAME", T."ID_MATERIAL", T."CODE", T."MIF_TEXT"
FROM "D:\GEOBLOCK\DATA\BASE\POLYGON\POLY\DZ2_Contours.db" T
ORDER BY T."PROFILE", T."OreType", T."OreBody", T."OreBlock" 
