  SELECT ADMIDATE,
  COUNT(*) admissions
  FROM HES_APC.dbo.[vHES_APC_Flat]

  WHERE (DIAG4_01 = 'U071'
      OR DIAG4_02 = 'U071'
      OR DIAG4_03 = 'U071'
      OR DIAG4_04 = 'U071'
      OR DIAG4_05 = 'U071'
      OR DIAG4_06 = 'U071'
      OR DIAG4_07 = 'U071'
      OR DIAG4_08 = 'U071'
      OR DIAG4_09 = 'U071'
      OR DIAG4_10 = 'U071'
      OR DIAG4_11 = 'U071'
      OR DIAG4_12 = 'U071'
      OR DIAG4_13 = 'U071'
      OR DIAG4_14 = 'U071'
      OR DIAG4_15 = 'U071'
      OR DIAG4_16 = 'U071'
      OR DIAG4_17 = 'U071'
      OR DIAG4_18 = 'U071'
      OR DIAG4_19 = 'U071'
      OR DIAG4_20 = 'U071'
      OR DIAG4_01 = 'U073'
      OR DIAG4_02 = 'U073'
      OR DIAG4_03 = 'U073'
      OR DIAG4_04 = 'U073'
      OR DIAG4_05 = 'U073'
      OR DIAG4_06 = 'U073'
      OR DIAG4_07 = 'U073'
      OR DIAG4_08 = 'U073'
      OR DIAG4_09 = 'U073'
      OR DIAG4_10 = 'U073'
      OR DIAG4_11 = 'U073'
      OR DIAG4_12 = 'U073'
      OR DIAG4_13 = 'U073'
      OR DIAG4_14 = 'U073'
      OR DIAG4_15 = 'U073'
      OR DIAG4_16 = 'U073'
      OR DIAG4_17 = 'U073'
      OR DIAG4_18 = 'U073'
      OR DIAG4_19 = 'U073'
      OR DIAG4_20 = 'U073')

AND EPIORDER = 1
AND FYEAR IN (1819, 1920, 2021, 2122)

GROUP BY ADMIDATE, PROCODE3, SITETRET
	  
