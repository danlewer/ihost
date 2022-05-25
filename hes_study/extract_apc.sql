SELECT TOKEN_PERSON_ID, EPIKEY, STARTAGE, SEX, ADMIDATE, ADMIMETH, EPIORDER, DISDATE, DISMETH, PROCODE, SITETRET, DIAG4_01, DIAG4_02, DIAG4_03, DIAG4_04, DIAG4_05, DIAG4_06, DIAG4_07, DIAG4_08, DIAG4_09, DIAG4_10, DIAG4_11, DIAG4_12, DIAG4_13, DIAG4_14, DIAG4_15, DIAG4_16, DIAG4_17, DIAG4_18, DIAG4_19, DIAG4_20
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE EPIKEY IN (SELECT diag.EPIKEY 
				FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
				WHERE diag.DiagCode4 LIKE 'F11%')
AND FYEAR IN ('1718', '1819', '1920', '2021', '2122')
AND EPISTAT = 3
