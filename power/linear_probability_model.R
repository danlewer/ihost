# TO DO
# what about NA disdates or 1801-01-01? (impute some with speldur?)
# fix drug-related codes
# add COVID-19 beds as hospital-level variable
# check that PROCODE3 and SITETRET are being used correctly
# impute STARTAGE
# 

library(data.table)
library(stringi)
library(lubridate)
library(meta)
library(lme4)
library(RColorBrewer)
library(extrafont)
loadfonts(device = 'win')

propCI <- function(X, N, ..., form = T, digs = 0, pc = T, col.names = c('prop', 'lower', 'upper')) {
  r <- t(rbind(X/N, mapply(function(x, n) prop.test(x, n, ...)$conf.int[1:2], x = X, n = N)))
  colnames(r) <- col.names
  if (form == T) {
    if (pc == T) {r <- r * 100}
    r <- format(round(r, digs), nsmall = digs, digits = digs)
    r <- paste0(r[,1], '(', r[,2], '-', r[,3], ')')
    gsub('\\(', ' (', gsub(' ', '', r))
  } else {
    r
  }
}

prop2CI <- function (x1, x2, n1, n2, col.names = c('prop1', 'prop2', 'diff', 'lower', 'upper')) {
  x <- asplit(cbind(x1, x2), MARGIN = 1)
  n <- asplit(cbind(n1, n2), MARGIN = 1)
  r <- mapply(prop.test, x = x, n = n, SIMPLIFY = F)
  r <- sapply(r, function (x) c(x$estimate, d = diff(x$estimate), -x$conf.int[1:2]))
  r <- t(r)
  `colnames<-`(r, col.names)
}

mli <- function (x, digs = 0) {
  q <- quantile(x, probs = c(0.5, 0.25, 0.75), na.rm = T)
  q <- format(round(q, digs), nsmall = digs, digits = digs)
  paste0(q[1], ' (', q[2], '-', q[3], ')')
}

# secure folder location
setwd("Z:/IA_UserData/dan.lewer/ihost")

d <- fread("ihost_test_full_export_25may2022.csv")
#a <- fread("ihost_ae_export_13may2022.csv")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# check if F11 is in diagnosis field at least once for each individual
# ....................................................................

diags <- paste0('DIAG4_', stri_pad(1:20, width = 2, pad = '0'))
f11 <- d[, lapply(.SD, stri_sub, from = 1L, to = 3L), .SDcols = diags]
f11 <- f11[, lapply(.SD, function (x) x == 'F11')]
d[, f11 := rowSums(f11)]
f11 <- d[, .(f11s = sum(f11)), TOKEN_PERSON_ID]
f11[, .N, f11s == 0]
# those with zero F11's must have F11's in historical episodes

# :::::::::::::::::
# process variables
# .................

# format variables
# ................

d[, ADMIDATE := ymd(ADMIDATE)]
d[, DISDATE := ymd(DISDATE)]
fyear_lim <- make_date(year = 2017:2022, month = 4, day = 1)
d[, FYEAR := findInterval(as.integer(ADMIDATE), as.integer(fyear_lim))]
d[, FYEAR := factor(FYEAR, seq_along(fyear_lim), paste0(17:22, 18:23))]
d[, FYEAR := as.character(FYEAR)]
d[, STARTAGE := as.numeric(STARTAGE)]
d$STARTAGE[d$STARTAGE > 120] <- NA
d[, PROCODE3 := stri_sub(PROCODE, 1, 3)]
d[, SEX := factor(SEX, c(1, 2, 0, 9), c('male', 'female', 'female', 'female'))]

# impute STARTAGE
# ...............

d[, birthYear := year(ADMIDATE) - STARTAGE]
d$birthYear[d$birthYear < 1930 | d$birthYear > 2020] <- NA_integer_
birthYear <- d[, .(birthYear = round(mean(birthYear, na.rm = T), 0)), TOKEN_PERSON_ID]
d[, birthYear := NULL]
d <- birthYear[d, on = 'TOKEN_PERSON_ID']
d[, estAge := year(ADMIDATE) - birthYear]
d[, STARTAGE2 := fifelse(is.na(STARTAGE), estAge, STARTAGE)]
d[, table(is.na(STARTAGE), is.na(STARTAGE2))] # minor benefit

# add NHS Trust and hospital names
# ................................

# PROCODE3 / NHS Trust
# Leeds Teaching Hospitals NHS Foundation Trust: RR8 
# University College London Hospitals NHS Foundation Trust: RRV
# University Hospitals of North Midlands NHS Trust: RJE

# SITETRET / Hospital site
# St James's University Hospital (LTHFT): RR813 
# University College Hospital (UCLH): RRV03
# Royal Stoke University Hospital (UHNM): RJE01

procode3_lookup <- read.csv(url('https://raw.githubusercontent.com/danlewer/ihost/main/hes_study/lookups/procode3.csv'), col.names = c('FYEAR', 'PROCODE3', 'NHST'), colClasses = 'character')
sitetret_lookup <- read.csv(url('https://raw.githubusercontent.com/danlewer/ihost/main/hes_study/lookups/sitetret.csv'), col.names = c('FYEAR', 'SITETRET', 'hosp'), colClasses = 'character')
setDT(procode3_lookup); setDT(sitetret_lookup)
d <- procode3_lookup[d, on = c('FYEAR', 'PROCODE3')]
d <- sitetret_lookup[d, on = c('FYEAR', 'SITETRET')]

# icd10 lookup for primary cause of admission
# ...........................................

gr <- read.csv(url('https://raw.githubusercontent.com/danlewer/ihost/main/hes_study/lookups/icd103l_grouper.csv'))
setDT(gr)
d[, icd103 := stri_sub(DIAG4_01, 0, 3)]
d <- gr[d, on = 'icd103']

# :::::::::::::::
# drop ineligible
# ...............

d <- d[!is.na(ADMIDATE)] # none
d <- d[ADMIDATE >= as.Date('2017-04-01', origin = '1970-01-01')]
d[, .N, FYEAR]
d <- d[is.na(STARTAGE2) | (STARTAGE2 >= 18 & STARTAGE2 <= 64)] # allow missing age

# ::::::::::::::::::
# process admissions
# ..................

# admission and discharge dates
# .............................

d2 <- unique(d[, c('TOKEN_PERSON_ID', 'ADMIDATE')])
d2[, aid := .I]
d <- d2[d, on = c('TOKEN_PERSON_ID', 'ADMIDATE')]
d <- d[, .(minOrder = min(EPIORDER)), aid][d, on = 'aid']
d2 <- d[, .(DISDATE = max(DISDATE, na.rm = T)), aid][d2, on = 'aid']
d2[, dur := as.integer(DISDATE - ADMIDATE) + 1L]
d2[, FYEAR := findInterval(as.integer(ADMIDATE), as.integer(fyear_lim))]
d2[, FYEAR := factor(FYEAR, seq_along(fyear_lim), paste0(17:22, 18:23))]
d2[, FYEAR := as.character(FYEAR)]

# age, sex
# ........

d2 <- d[, .(STARTAGE = min(STARTAGE2, na.rm = T)), aid][d2, on = 'aid']
d2$STARTAGE[d2$STARTAGE == Inf] <- NA
d2 <- d[EPIORDER == minOrder, c('aid', 'SEX')][d2, on = 'aid']

# admission and discharge methods
# ...............................

# 1 = emergency; 2 = planned; 3 = other
d[, epo := 3L]
d$epo[d$ADMIMETH %in% c(21:25, '2A', '2B', '2C', '2D', 28)] <- 1L
d$epo[d$ADMIMETH %in% 11:13] <- 2L  
d2 <- d[, .(epo = min(epo, na.rm = T)), aid][d2, on = 'aid']

# 1 = died; 2 = self; 3 = normal; 4 = other/unknown
d[, dis := 4L]
d$dis[d$DISMETH == 4] <- 1L
d$dis[d$DISMETH == 2] <- 2L
d$dis[d$DISMETH == 1] <- 3L
d2 <- d[, .(dis = min(dis, na.rm = T)), aid][d2, on = 'aid']

# primary diagnosis
# .................

# choose first diagnosis that is not R69X

d[, R69 := grepl('R69', DIAG4_01)]
d <- d[order(aid, R69, EPIORDER)]
d[, diagID := rowid(aid)]
d2 <- d[diagID == 1, c('aid', 'icd103', 'DIAG4_01')][d2, on = 'aid']
d2 <- gr[, c('icd103', 'chapter', 'l1')][d2, on = 'icd103']
d2[, chapter := sub(" .*", "", chapter)]

# group primary diagnosis

drug <- c(paste0('F', 10:19), paste0('T', 36:50))
irid <- c('L02', 'L03', 'I80', 'A48', 'L088', 'L089', 'L97', 'L984', 'L988', 'L989', 'R02', 'B376', 'I330', 'I339', 'I38', 'I39', 'A40', 'A41', 'R572', 'B377', 'M86', 'M00', 'M465', 'M762')
d2[, cause := 'other']
d2$cause[d2$chapter %in% c('III', 'IV', 'VI', 'VII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV')] <- 'other_NCDs'
d2$cause[d2$chapter %in% c('XIX', 'XX')] <- 'other_accidents'
d2$cause[d2$chapter == 'I'] <- 'other_infections'
d2$cause[d2$chapter == 'V'] <- 'mental'
d2$cause[d2$chapter == 'II'] <- 'cancers'
d2$cause[d2$chapter == 'XVIII'] <- 'signs'
d2$cause[grepl(paste0(irid, collapse = '|'), d2$DIAG4_01)] <- 'irid'
d2$cause[grepl(paste0(drug, collapse = '|'), d2$DIAG4_01)] <- 'drug'
d2[, cause := factor(cause, c('other_NCDs', 'cancers', 'drug', 'irid', 'mental', 'other', 'other_accidents', 'other_infections', 'signs'))]

# procode and sitetret
# ....................

# few have varying values - take first one

d <- d[order(aid, EPIORDER)]
d[, hospID := rowid(aid)]
d2 <- d[hospID == 1, c('aid', 'PROCODE3', 'SITETRET')][d2, on = 'aid']
d2 <- procode3_lookup[d2, on = c('PROCODE3', 'FYEAR')]
d2 <- sitetret_lookup[d2, on = c('SITETRET', 'FYEAR')]

# list of diagnoses per admission
# ...............................

dl <- d[, c('aid', diags), with = F]
dl <- melt(dl, id.vars = 'aid', variable.name = 'pos', value.name = 'icd4')
dl <- dl[icd4 != 'NULL']
dl[, pos := NULL]
dl <- unique(dl)
dl[, icd103 := stri_sub(icd4, 0, 3)]
dl <- gr[, c('icd103', 'chapter')][dl, on = 'icd103']
dl[, chapter := sub(" .*", "", chapter)]

# any F11
# -------

d2[, anyF11 := aid %in% dl[icd103 == 'F11', aid]]
d2[, anyF11 := anyF11 | icd103 == 'F11']

# comorbidity count
# .................

co_chaps <- c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV')
cmscore <- dl[chapter %in% co_chaps, c('aid', 'chapter')]
cmscore <- unique(cmscore)
cmscore <- cmscore[, .(cmscore = .N), aid]
d2 <- cmscore[d2, on = 'aid']
d2$cmscore[is.na(d2$cmscore)] <- 0L

# calendar years
# ..............

d2[, yr := year(ADMIDATE)]

# ::::::::::::::::::::::::::::
# identify relevant admissions
# ............................

f11year <- d2[anyF11 == T, .(TOKEN_PERSON_ID = TOKEN_PERSON_ID, f11_date = ADMIDATE)][d2[, c('TOKEN_PERSON_ID', 'aid', 'ADMIDATE')], on = 'TOKEN_PERSON_ID', allow.cartesian = T]
f11year[, include := ADMIDATE < (f11_date + 365)]
f11year <- f11year[, .(include = any(include)), aid]
d2 <- f11year[d2, on = 'aid']
d2$include[is.na(d2$include)] <- F

# ::::::::::::::::::::::::::::::::::::::::::::::::::::
# top 50 acute hospitals with stable admission numbers
# ....................................................

# use UCLH rather than UCH

ihost_sites <- c('UNIVERSITY COLLEGE HOSPITAL', 'ROYAL STOKE UNIVERSITY HOSPITAL', "ST JAMES'S UNIVERSITY HOSPITAL")
d2[, hosp2 := fifelse(PROCODE3 == 'RRV', 'UNIVERSITY COLLEGE HOSPITAL', hosp)]

# > 100 patients in total

d2[, hosp100 := hosp2 %in% d2[epo == 1 & include == T, .N, hosp2][N >= 100, hosp2]]

# stability of admissions 2017:2021
yr_change <- dcast(d2[hosp100 == T & yr %in% 2017:2021], hosp2 ~ yr, value.var = 'hosp2', fun.aggregate = length)
pc_change <- function (yr) yr_change[, (get(as.character(yr + 1)) - get(as.character(yr))) / get(as.character(yr))]
pc_change <- `colnames<-`(as.data.table(lapply(2017:2020, pc_change)), paste0(2017:2020, '_pc'))
pc_change[is.na(pc_change)] <- Inf
yr_change[, unstable := rowSums(pc_change < -0.5 | pc_change > 2) > 0]
d2 <- yr_change[, c('hosp2', 'unstable')][d2, on = 'hosp2']

top50 <- d2[epo == 1 & hosp != 'NULL' & yr == 2021 & unstable == F, .N, hosp2][order(N, decreasing = T)]
top50 <- top50[!(hosp2 %in% ihost_sites), .(hosp2 = hosp2, hpseu = .I)][top50, on = 'hosp2']
top50$hpseu[top50$hosp2 == ihost_sites[1]] <- 'A'
top50$hpseu[top50$hosp2 == ihost_sites[2]] <- 'B'
top50$hpseu[top50$hosp2 == ihost_sites[3]] <- 'C'
top50 <- top50[hpseu %in% c('A', 'B', 'C', 1:50)]
top50[, hpseu := factor(hpseu, c('A', 'B', 'C', 1:50))]

d2 <- top50[, c('hosp2', 'hpseu')][d2, on = 'hosp2']

# ::::::::::
# covid beds
# ..........

covid_beds <- read.csv(url('https://raw.githubusercontent.com/danlewer/ihost/main/hes_study/covid_beds.csv'))
setDT(covid_beds)
covid_beds <- melt(covid_beds, id.vars = c('region', 'code', 'name', 'covid'), variable.name = 'date', value.name = 'beds')
covid_beds[, beds := as.integer(beds)]
covid_beds <- dcast(covid_beds, region + code + name + date ~ covid, value.var = 'beds')
covid_beds[, date := gsub('X', '', date)]
covid_beds[, date := ymd(date)]
covid_beds[, pc := covid / all]
covid_beds[, yr := year(date)]

covid_beds2021 <- covid_beds[nchar(code) == 3 & yr %in% c(2020, 2021), .(all = sum(all), covid = sum(covid)), c('code', 'yr')]
covid_beds2021[, covid_pc := covid / all]
setnames(covid_beds2021, 'code', 'PROCODE3')

map_procode_to_hosp <- d2[!is.na(hpseu), .N, c('hpseu', 'PROCODE3')][order(hpseu, -N)]
map_procode_to_hosp[, i := rowid(hpseu)]
map_procode_to_hosp <- map_procode_to_hosp[i == 1, c('hpseu', 'PROCODE3')]
covid_beds2021 <- map_procode_to_hosp[covid_beds2021, on = 'PROCODE3']

# ::::::::::::::::
# analysis dataset
# ................

d3 <- d2[include == T & epo == 1 & !is.na(hpseu)]
d3[, ama := dis == 2]

# ::::::::::::::::::::::::
# next emergency admission
# ........................

nextEmergency <- unique(d2[epo == 1, .(TOKEN_PERSON_ID = TOKEN_PERSON_ID, nextAPC = ADMIDATE)])
nextEmergency <- nextEmergency[unique(d3[, c('TOKEN_PERSON_ID', 'ADMIDATE')]), on = 'TOKEN_PERSON_ID', allow.cartesian = T]
nextEmergency <- nextEmergency[nextAPC > ADMIDATE]
nextEmergency <- nextEmergency[, .(nextAPC = min(nextAPC)), c('TOKEN_PERSON_ID', 'ADMIDATE')]
d3 <- nextEmergency[d3, on = c('TOKEN_PERSON_ID', 'ADMIDATE')]
d3[, APC28 := as.integer(nextAPC - ADMIDATE) < 29]
d3$APC28[is.na(d3$APC28)] <- F

# ::::::::::::::::::::::::
# baseline characteristics
# ........................

baseline <- d3[yr == 2021, .(
  n = .N,
  ama = sum(dis == 2),
  ama_prop = propCI(sum(dis == 2), .N, digs = 1),
  APC28 = sum(APC28),
  APC28_prop = propCI(sum(APC28), .N, digs = 1),
  ageIQR = mli(STARTAGE),
  male = sum(SEX == 'male'),
  pc_male = propCI(sum(SEX == 'male'), .N, digs = 1),
  cm = mli(cmscore),
  cm5 = sum(cmscore >= 5),
  cm5_prop = propCI(sum(cmscore >= 5), .N, digs = 1),
  drug = sum(cause == 'drug'),
  drug_prop = propCI(sum(cause == 'drug'), .N, digs = 1),
  irid = sum(cause == 'irid'),
  irid_prop = propCI(sum(cause == 'irid'), .N, digs = 1),
  other_NCDs = sum(cause == 'other_NCDs'),
  other_NCDs_prop = propCI(sum(cause == 'other_NCDs'), .N, digs = 1)
  ), hpseu]

all_controls <- d3[hpseu %in% 1:50 & yr == 2021, .(
  n = .N,
  ama = sum(dis == 2),
  ama_prop = propCI(sum(dis == 2), .N, digs = 1),
  APC28 = sum(APC28),
  APC28_prop = propCI(sum(APC28), .N, digs = 1),
  ageIQR = mli(STARTAGE),
  male = sum(SEX == 'male'),
  pc_male = propCI(sum(SEX == 'male'), .N, digs = 1),
  cm = mli(cmscore),
  cm5 = sum(cmscore >= 5),
  cm5_prop = propCI(sum(cmscore >= 5), .N, digs = 1),
  drug = sum(cause == 'drug'),
  drug_prop = propCI(sum(cause == 'drug'), .N, digs = 1),
  irid = sum(cause == 'irid'),
  irid_prop = propCI(sum(cause == 'irid'), .N, digs = 1),
  other_NCDs = sum(cause == 'other_NCDs'),
  other_NCDs_prop = propCI(sum(cause == 'other_NCDs'), .N, digs = 1)
)]

all_controls <- cbind(hpseu = 'all_controls', all_controls)
baseline <- rbind(baseline, all_controls)
baseline[, hpseu := factor(hpseu, c('A', 'B', 'C', 'all_controls', 1:50))]
baseline <- baseline[order(hpseu)]

# add covid beds

baseline <- covid_beds2021[!is.na(hpseu) & yr == 2021, c('hpseu', 'covid_pc')][baseline, on = 'hpseu']
covid_all_controls <- covid_beds2021[!is.na(hpseu) & yr == 2021 & !(hpseu %in% c('A', 'B', 'C')), sum(covid, na.rm = T) / sum(all, na.rm = T)]
baseline$covid_pc[baseline$hpseu == 'all_controls'] <- covid_all_controls
baseline[, covid_pc := format(round(covid_pc * 100, 1), nsmall = 1, digits = 1)]

fwrite(baseline, 'baseline2021_25may2022.csv')

# histogram of age

png('histogram_of_age_11may2022.png', height = 7, width = 7, units = 'in', res = 300)
hist(d3$STARTAGE, main = NA, xlab = 'Age at admission')
dev.off()

# ::::::::::::::::::::::::::::
# test analysis: 2020 vs. 2021
# ............................

test <- d3[yr %in% c(2020, 2021)]
test[, ihost := hpseu %in% c('A', 'B', 'C')]
test[, ba := factor(yr, c(2020, 2021), c('before', 'after'))]
test <- covid_beds2021[!is.na(hpseu), c('hpseu', 'yr', 'covid_pc')][test, on = c('hpseu', 'yr')]
test$covid_pc[is.na(test$covid_pc) & !test$ihost] <- covid_all_controls

# describe rates of AMA before and after
# ......................................

test[, hosp3 := fifelse(ihost == T, as.character(hpseu), 'control')]
test[, .(n = .N, ama = sum(ama), pcama = mean(ama)), c('ba', 'hosp3')]
test_describe <- test[, .(n = .N, ama = sum(ama), apc28 = sum(APC28)), c('ba', 'hosp3')]
test_describe <- cbind(test_describe, 
                       propCI(test_describe$ama, test_describe$n, form = F, col.names = c('ama_prop', 'ama_lower', 'ama_upper')),
                       propCI(test_describe$apc28, test_describe$n, form = F, col.names = c('apc28_prop', 'apc28_lower', 'apc28_upper')))
test_describe <- test_describe[order(hosp3)]
test_describe[, x := rowid(ba)]
test_describe$x <- test_describe$x + ifelse(test_describe$ba == 'before', -0.2, 0.2)

cols <- brewer.pal(3, 'Spectral')[1:2]
ys <- seq(0.15, 0.2, length.out = 3)

png('outcomes_2020_vs_2021.png', height = 12, width = 7, units = 'in', res = 300)

par(mar = c(4, 4, 1, 10), mfrow = c(2, 1), xpd = NA)

plot(1, type = 'n', xlim = c(0.5, 4.5), ylim = c(0, 0.4), axes = F, xlab = NA, ylab = 'Discharge against medical advice')
with(test_describe, {
  rect(x - 0.2, 0, x + 0.2, ama_prop, col = cols)
  arrows(x, ama_lower, x, ama_upper, length = 0.07, angle = 90, code = 3) })
axis(2, seq(0, 0.4, 0.05), paste0(seq(0, 0.4, 0.05) * 100, ' %'), las = 2, pos = 0.5)
axis(1, 1:4, c('A', 'B', 'C', 'Controls'), tick = F)
segments(0.5, 0, x1 = 4.5)
rect(4.6, ys[-length(ys)], 4.8, ys[-1], col = rev(cols))
text(4.9, ys[-length(ys)] + diff(ys)/2, c('2021', '2020'), adj = 0)

plot(1, type = 'n', xlim = c(0.5, 4.5), ylim = c(0, 0.4), axes = F, xlab = NA, ylab = '28 day readmission')
with(test_describe, {
  rect(x - 0.2, 0, x + 0.2, apc28_prop, col = cols)
  arrows(x, apc28_lower, x, apc28_upper, length = 0.07, angle = 90, code = 3) })
axis(2, seq(0, 0.4, 0.05), paste0(seq(0, 0.4, 0.05) * 100, ' %'), las = 2, pos = 0.5)
axis(1, 1:4, c('A', 'B', 'C', 'Controls'), tick = F)
segments(0.5, 0, x1 = 4.5)
rect(4.6, ys[-length(ys)], 4.8, ys[-1], col = rev(cols))
text(4.9, ys[-length(ys)] + diff(ys)/2, c('2021', '2020'), adj = 0)

dev.off()

# rates before and after for all hospitals
# ........................................

ba_all <- test[ba == 'before', .(n_before = .N, ama_before = sum(ama), apc28_before = sum(APC28)), hpseu][
  test[ba == 'after', .(n_after = .N, ama_after = sum(ama), apc28_after = sum(APC28)), hpseu], on = 'hpseu']
ba_all <- cbind(ba_all, 
                prop2CI(x1 = ba_all$ama_before, n1 = ba_all$n_before, x2 = ba_all$ama_after, n2 = ba_all$n_after, col.names = c('p_ama_before', 'p_ama_after', 'diff_ama', 'diff_ama_lower', 'diff_ama_upper')),
                prop2CI(x1 = ba_all$apc28_before, n1 = ba_all$n_before, x2 = ba_all$apc28_after, n2 = ba_all$n_after, col.names = c('p_apc28_before', 'p_apc28_after', 'diff_apc28', 'diff_apc28_lower', 'diff_apc28_upper')))
ba_all[, cl := fifelse(hpseu %in% c('A', 'B', 'C'), 'red', 'black')]

png('all_hosps_2020_2021.png', height = 9, width = 7, units = 'in', res = 300)

par(mfrow = c(2, 1), mar = c(1, 3, 1, 2))

ba_all <- ba_all[order(diff_ama, decreasing = T)]
plot(1, type = 'n', xlim = c(0, 54), ylim = c(-0.2, 0.2), axes = F, xlab = NA, ylab = NA)
rect(0, -0.2, 54, 0.2)
segments(0, 0, x1 = 54)
with(ba_all, {
  points(1:53, diff_ama, pch = 19, col = cl)
  arrows(1:53, diff_ama_lower, 1:53, diff_ama_upper, angle = 90, length = 0.03, code = 3, col = cl)
})
axis(2, seq(-0.2, 0.2, 0.025), labels = paste0(seq(-0.2, 0.2, 0.025) * 100, '%'), pos = 0, las = 2)

ba_all <- ba_all[order(diff_apc28, decreasing = T)]
plot(1, type = 'n', xlim = c(0, 54), ylim = c(-0.2, 0.2), axes = F, xlab = NA, ylab = NA)
rect(0, -0.2, 54, 0.2)
segments(0, 0, x1 = 54)
with(ba_all, {
  points(1:53, diff_apc28, pch = 19, col = cl)
  arrows(1:53, diff_apc28_lower, 1:53, diff_apc28_upper, angle = 90, length = 0.03, code = 3, col = cl)
})
axis(2, seq(-0.2, 0.2, 0.025), labels = paste0(seq(-0.2, 0.2, 0.025) * 100, '%'), pos = 0, las = 2)

dev.off()

# longer time-series of outcomes
# ..............................

outcome_series <- d3[yr %in% c(2017:2021), .(n = .N, ama = sum(dis == 2), apc28 = sum(APC28)), c('yr', 'hpseu')]
outcome_series <- cbind(outcome_series, 
                        propCI(X = outcome_series$ama, N = outcome_series$n, form = F, col.names = c('ama_prop', 'ama_lower', 'ama_upper')),
                        propCI(X = outcome_series$apc28, N = outcome_series$n, form = F, col.names = c('apc28_prop', 'apc28_lower', 'apc28_upper')))
outcome_series <- outcome_series[order(hpseu, yr)]
outcome_series <- split(outcome_series, f = outcome_series$hpseu)

par(mfrow = c(5, 5), mar = c(0, 0, 0, 0))
lapply(outcome_series[1:25], function (x) {
  plot(1, type = 'n', xlim = c(2016, 2022), ylim = c(0, 0.25), xlab = NA, ylab = NA, axes = F)
  box()
  with(x, {
    points(yr, ama_prop, pch = 19, type = 'b')
    arrows(yr, ama_lower, yr, ama_upper, angle = 90, length = 0.03, code = 3)
  })
})

par(mfrow = c(5, 5), mar = c(0, 0, 0, 0))
lapply(outcome_series[1:25], function (x) {
  plot(1, type = 'n', xlim = c(2016, 2022), ylim = c(0, 0.5), xlab = NA, ylab = NA, axes = F)
  box()
  with(x, {
    points(yr, apc28_prop, pch = 19, type = 'b')
    arrows(yr, apc28_lower, yr, apc28_upper, angle = 90, length = 0.03, code = 3)
  })
})


# linear probability model
# ........................

# crude model without covariates

tmp <- test[!(hpseu %in% c('B', 'C'))]
lpm <- dcast(tmp, ihost ~ ba, value.var = 'ama', fun.aggregate = mean)
lpm[, d := after - before]
lpm[, diff(d)] * 100
mAl <- lm(ama ~ ba*ihost, data = tmp)
tail(coef(mAl), 1) * 100 # works

tmp[, pred := mAl$fitted.values]
table(tmp$pred)

# models with covariates

lpm <- function (outcome = 'ama', DATA, covs = "+ STARTAGE + SEX + cmscore + cause + covid_pc") {
  m <- lapply(c('A', 'B', 'C'), function (x) {
    f <- as.formula(paste0(outcome, "~ ba*ihost", covs))
    lm(f, data = DATA[hpseu %in% c(x, 1:50)])
  })
  data.frame(TE = sapply(m, function (x) tail(summary(x)$coef[,1], 1)),
             seTE = sapply(m, function (x) tail(summary(x)$coef[,2], 1)),
             p = sapply(m, function (x) tail(summary(x)$coef[,4], 1)))
}

test_meta_ama <- lpm(outcome = 'ama', DATA = test)
test_meta_apc <- lpm(outcome = 'APC28', DATA = test)

test_meta_ama <- metagen(TE = test_meta_ama$TE, seTE = test_meta_ama$seTE, sm = "RD", studlab = c('A', 'B', 'C'))
test_meta_apc <- metagen(TE = test_meta_apc$TE, seTE = test_meta_apc$seTE, sm = "RD", studlab = c('A', 'B', 'C'))

par(mfrow = c(2, 1))
forest(test_meta_ama, comb.random = F, digits = 3)
forest(test_meta_apc, comb.random = F, digits = 3)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# simulation: 2020 is baseline year; outcomes for 2021 are simulated
# ..................................................................

# example single simulations
# ..........................

sim <- function (baselineYear = 2020, outcome = 'ama', ihost_effect = 0.05) {
  a <- d3[yr %in% c(baselineYear, baselineYear + 1)]
  a$ba <- factor(a$yr, c(baselineYear, baselineYear + 1), c('before', 'after'))
  a$ihost <- 'control'
  a$ihost[a$hpseu %in% c('A', 'B', 'C')] <- 'ihost'
  baseline_risk <- a[yr == baselineYear, .(baseline = mean(get(outcome))), hpseu]
  baseline_risk$after_risk <- ifelse(baseline_risk$hpseu %in% c('A', 'B', 'C'), baseline_risk$baseline - ihost_effect, baseline_risk$baseline)
  a <- baseline_risk[a, on = 'hpseu']
  a$simOutcome <- rbinom(nrow(a), 1, a$after_risk) == 1
  a$simOutcome <- ifelse(a$ba == 'before', a[, get(outcome)], a$simOutcome)
  m <- list(A = lmer(simOutcome ~ ba * ihost + STARTAGE + SEX + cmscore + cause + (1|hpseu), data = a[hpseu %in% c(1:50, 'A')]),
            B = lmer(simOutcome ~ ba * ihost + STARTAGE + SEX + cmscore + cause + (1|hpseu), data = a[hpseu %in% c(1:50, 'B')]),
            C = lmer(simOutcome ~ ba * ihost + STARTAGE + SEX + cmscore + cause + (1|hpseu), data = a[hpseu %in% c(1:50, 'C')]))
  sapply(m, function (x) c(TE = tail(summary(x)$coef[,1], 1), seTE = tail(summary(x)$coef[,2], 1)))
}

# ama
set.seed(666)
ama_eg <- sim(outcome = 'ama', ihost_effect = 0.05)
ama_egM <- metagen(TE = ama_eg[1,], seTE = ama_eg[2,], sm = 'RD')
png('example_forest_ama_6june2022.png', height = 4, width = 8, units = 'in', res = 300, family = 'Segoe UI Light')
forest(ama_egM, comb.random = F, digits = 3, studlab = c('A', 'B', 'C'), leftcols = c("studlab", "effect", "ci", "w.fixed"), rightcols = F)
dev.off()

# readmission
set.seed(13) # 10 is a bit too good
rea_eg <- sim(outcome = 'APC28', ihost_effect = 0.05)
rea_egM <- metagen(TE = rea_eg[1,], seTE = rea_eg[2,], sm = 'RD')
png('example_forest_rea_6june2022.png', height = 4, width = 8, units = 'in', res = 300, family = 'Segoe UI Light')
forest(rea_egM, comb.random = F, digits = 3, studlab = c('A', 'B', 'C'), leftcols = c("studlab", "effect", "ci", "w.fixed"), rightcols = F)
dev.off()

# power
# .....

sim <- function (baselineYear = 2020, outcome = 'ama', ihost_effect = 0.05, nSim = 10L) {
  a <- d3[yr %in% c(baselineYear, baselineYear + 1)]
  a$ba <- factor(a$yr, c(baselineYear, baselineYear + 1), c('before', 'after'))
  a$ihost <- 'control'
  a$ihost[a$hpseu %in% c('A', 'B', 'C')] <- 'ihost'
  baseline_risk <- a[yr == baselineYear, .(baseline = mean(get(outcome))), hpseu]
  baseline_risk$after_risk <- ifelse(baseline_risk$hpseu %in% c('A', 'B', 'C'), baseline_risk$baseline - ihost_effect, baseline_risk$baseline)
  a <- baseline_risk[a, on = 'hpseu']
  lapply(seq_len(nSim), function (i) {
    print (i)
    a$simOutcome <- rbinom(nrow(a), 1, a$after_risk) == 1
    a$simOutcome <- ifelse(a$ba == 'before', a[, get(outcome)], a$simOutcome)
    abc <- lpm(outcome = 'simOutcome', DATA = a, covs = "+ STARTAGE + SEX + cmscore + cause")
    met <- metagen(TE = abc$TE, seTE = abc$seTE, sm = 'RD', studlab = c('A', 'B', 'C'))
    rbind(abc,
          c(met$TE.fixed, met$seTE.fixed, met$pval.fixed),
          c(met$TE.random, met$seTE.random, met$pval.random))
  })
}

titles <- c('Site A', 'Site B', 'Site C', 'Pooled effect')

set.seed(455)

# DAMA
sim2020ama <- sim(outcome = 'ama', nSim = 1000L)
sapply(`names<-`(1:4, titles), function (x) mean(sapply(sim2020ama, function (y) y[x,3]) < 0.05))

# Emergency readmission
sim2020apc <- sim(outcome = 'APC28', nSim = 1000L)
sapply(`names<-`(1:4, titles), function (x) mean(sapply(sim2020apc, function (y) y[x,3]) < 0.05))

# plots

png('simulated_effect_histograms_11may2022.png', height = 8, width = 7, units = 'in', res = 300)

layout(mat = matrix(1:8, ncol = 2))
par(xpd = NA, mar = c(3, 3, 2, 1))
for (j in list(sim2020ama, sim2020apc)) {
  for (i in 1:4) {
    hist(sapply(j, function (x) x[i,1]), breaks = seq(-0.15, 0.05, 0.005), main = NA, xlim = c(-0.15, 0.05), ylim = c(0, 250), xlab = NA, ylab = NA, axes = F)
    rect(-0.15, 0, 0.05, 250)
    effect <- mean(sapply(j, function (x) x[i,1]))
    power <- mean(sapply(j, function (x) x[i,3]) < 0.05)
    segments(effect, 0, y1 = 250)
    segments(0, 0, 0, 250)
    axis(1, pos = 0)
    axis(2, pos = -0.15, las = 2)
    text(effect, 270, paste0('mean RD = ', round(effect, 3)))
    text(0, 270, 'null')
    text(-0.145, 230, paste0('Power = ', round(power, 3)), adj = 0)
    text(-0.15, 270, titles[i], adj = 0, font = 2)
  }
}
  
dev.off()
