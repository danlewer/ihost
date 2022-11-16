# libraries and functions

library(data.table)

vpt <- function(x, t, cn = c('rate', 'lower', 'upper'), FUN = poisson.test) { # vectorized confidence intervals
  a <- mapply(FUN, x, t, SIMPLIFY = F)
  a <- sapply(a, function (x) c(x$estimate, x$conf.int[1:2]))
  `colnames<-`(t(a), cn)
}

# set working directory to location where data is saved

setwd("~/Documents/ihost")

# load data

d <- fread("kpi2.csv", col.names = c('PatientID', 'admission', 'q', 'med', 'dose', 'administration', 'los', 'discharge')) 

# format date

date_cols <- c('admission', 'administration', 'discharge')
d[, (date_cols) := lapply(.SD, dmy_hm), .SDcols = date_cols]

# order quarters

quarters <- c(t(outer(paste0(18:21, 19:22), paste0('_Q', 1:4), paste0)))
d[, q := factor(q, quarters)]

# create admission ID

d[, adID := .GRP, c('PatientID', 'admission')]

# expand timelines of doses

start_end <- d[, .(start = min(administration), end = max(administration)), c('adID', 'q')]
start_end[, duration := as_date(end) - as_date(start)]
timeline <- start_end[, .(start = seq(from = start, to = start + duration, by = 'day')), c('adID', 'q')]
timeline[, day := rowid(adID)]

# find which days doses were given

dose_days <- mapply(findInterval,
                    x = split(d$administration, f = d$adID),
                    vec = split(timeline$start, f = timeline$adID))
d[, day := unlist(dose_days)]

# add dose data to timeline

totalDose <- d[, .(totalDose = sum(dose)), c('adID', 'day')]
timeline <- totalDose[timeline, on = c('adID', 'day')]
timeline$totalDose[is.na(timeline$totalDose)] <- 0

# calculate KPIs

timeline <- timeline[, .(maxDose = max(totalDose)), adID][timeline, on = 'adID']
timeline[, maxToday := totalDose == maxDose]

KPIs <- timeline[, .(duration = max(day), maxDose = max(maxDose)), c('adID', 'q')][
  timeline[maxToday == T, .(maxDoseOnDay = min(day)), c('adID', 'q')], on = c('adID', 'q')]
KPIs[, KPI2a := maxDose >= 20]
KPIs[, KPI2b := fifelse(duration >= 3, maxDoseOnDay, NA_integer_)]

# report KPIs by quarter

KPIs_Q <- KPIs[, .(kpi2aN = .N, kpi2aX = sum(KPI2a), kpi2bN = sum(!is.na(KPI2b)), kpi2bX = sum(KPI2b >= 3, na.rm = T)), q]
KPIs_Q <- cbind(KPIs_Q, with(KPIs_Q, vpt(kpi2aX, kpi2aN, cn = c('kpi2a', 'kpi2a_lower', 'kpi2a_upper'), FUN = prop.test)))
KPIs_Q <- cbind(KPIs_Q, with(KPIs_Q, vpt(kpi2bX, kpi2bN, cn = c('kpi2b', 'kpi2b_lower', 'kpi2b_upper'), FUN = prop.test)))

# plots

KPIs_Q[, x := .I]

plot(1, type = 'n', xlim = c(0, 15), ylim = c(0.5, 1), axes = F, xlab = NA, ylab = NA)
with(KPIs_Q, {
  rect(x - 1, 0.5, x, kpi2a, col = "#8DD3C7")
  arrows(x - 0.5, kpi2a_lower, y1 = kpi2a_upper, code = 3, angle = 90, length = 0.05)
  axis(1, c(0, x), labels = F, pos = 0.5)
  axis(1, x - 0.5, q, las = 2, tick = F)
})
axis(2, 5:10 / 10, paste0(5:10 * 10, '%'), pos = 0, las = 2)
title(main = 'KPI2a: Proportion administered > 20mg in a 24 hour period')

plot(1, type = 'n', xlim = c(0, 15), ylim = c(0, 0.5), axes = F, xlab = NA, ylab = NA)
with(KPIs_Q, {
  rect(x - 1, 0, x, kpi2b, col = "#8DD3C7")
  arrows(x - 0.5, kpi2b_lower, y1 = kpi2b_upper, code = 3, angle = 90, length = 0.05)
  axis(1, c(0, x), labels = F, pos = 0)
  axis(1, x - 0.5, q, las = 2, tick = F)
})
axis(2, 0:5 / 10, paste0(0:5 * 10, '%'), pos = 0, las = 2)
title(main = 'KPI2b: Of patients staying at least 3 days, proportion taking 3+ days to reach max dose')
