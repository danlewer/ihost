  SELECT ADMIDATE, PROCODE3, SITETRET,
  COUNT(*) admissions
  FROM HES_APC.dbo.[vHES_APC_Flat]

  WHERE (DIAG4_01 IN ('U071', 'U072', 'U073', 'U109')
	  OR DIAG4_02 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_03 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_04 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_05 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_06 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_07 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_08 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_09 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_10 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_11 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_12 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_13 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_14 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_15 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_16 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_17 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_18 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_19 IN ('U071', 'U072', 'U073', 'U109')
      OR DIAG4_20 IN ('U071', 'U072', 'U073', 'U109')
	  )

	  AND EPIORDER = 1
	  AND FYEAR IN (1819, 1920, 2021, 2122)

GROUP BY ADMIDATE, PROCODE3, SITETRET
