SELECT DISMETH, SEX, STARTAGE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND STARTAGE > 17
AND STARTAGE < 110
AND FYEAR IN ('1819', '1920')
GROUP BY DISMETH, SEX, STARTAGE