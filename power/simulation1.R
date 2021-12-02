# this script specifies a difference-in-difference analysis and estimates the minimum detectable effect size with 80% power
# we assume 500 patients per iHOST site based on discussions with partners
# it includes assumptions about the number of eligible comparison sites (50) and patients per comparison site (50), though the power is not highly sensitive to these assumptions as the comparison group is so much larger than the iHOST group
# the model assumes observation periods 'before' and 'after' iHost, and outcomes are conceptualised as binomial risks
# the main analysis looks at an interaction between iHost site (yes/no) and before/after to estimate the association between iHost and the difference in outcome (i.e. difference in difference)
# we assume some variation in clinial practice between sites and therefore use a mixed model with random intercepts for hospital site
# the use of a mixed model in this simulation does not substantially affect power. It did identify that singularity may be a problem with model fitting, depending on the degree of variation between sites
# the simulation suggests 80% power to detect a risk ratio of 0.71, if all three iHOST sites are included
# if only one site is analysed in isolation, there is 80% power to detect a risk ratio of 0.57

#  :::::::::::::::::::::::
#  libraries and functions
#  .......................

library(lme4) # for mixed model
library(data.table)
library(RColorBrewer) # for figure colours
library(devEMF) # for EMF graphical device

#  ::::::
#  inputs
#  ......

# baseline risk

baseline_risk <- 0.15

# patient numbers

iHost <- 500
number_iHost_sites <- 3
per_comparison <- 50
number_comparisons <- 50

# intervention effects

baseline_difference <- 1.1 # iHost vs. comparison (assuming population at iHost sites is riskier)
time_trend <- 0.95 # secular change risk after/before. assuming improving clinical practice
site_variance <- 0.2 # determines site/cluster-level variance for comparison sites (standard deviation of log ratio)

# alpha & beta (for power calculation)

alpha <- 0.05
beta <- 0.8

#   :::::::::::::
#   simulate data
#   .............

sim <- function (iHost_effect = 0.6, number_iHost_sites = 3) {
  # labels
  intervention <- c(rep('iHost', times = iHost * number_iHost_sites * 2), rep('comparison', times = per_comparison * number_comparisons * 2))
  ba <- c('before', 'after')
  before_after <- c(rep(ba, each = iHost * number_iHost_sites), rep(ba, each = per_comparison * number_comparisons))
  before_after <- factor(before_after, ba)
  site <- c(rep(rep(seq_len(number_iHost_sites), each = iHost), times = 2), rep(rep(seq_len(number_comparisons), each = per_comparison), times = 2))
  # risks
  i_baseline_risk <- rep(baseline_risk, length(intervention))
  site_var <- rnorm(number_comparisons + number_iHost_sites, mean = 0, sd = site_variance)
  i_site_var <- exp(site_var[site])
  i_time_trends <- c(1, time_trend)[c(before_after == 'after') + 1]
  i_intervention_effect <- c(1, iHost_effect)[(intervention == 'iHost' & before_after == 'after') + 1]
  i_final_risk <- i_baseline_risk * i_site_var * i_time_trends * i_intervention_effect
  # outcome
  outcome <- rbinom(length(site), 1, i_final_risk)
  data.frame(intervention, site, before_after, i_baseline_risk, i_site_var, i_time_trends, i_intervention_effect, i_final_risk, outcome)
}

#  :::::::::::::::::::::::::
#  example single simulation
#  .........................

set.seed(13)
example_simulation <- sim(iHost_effect = 0.75)

#  check distributions (requires package data.table)
#  .................................................

setDT(example_simulation)
example_simulation[, .(n = .N, mn = mean(i_time_trends), v = var(i_time_trends)), c('intervention', 'before_after')]
example_simulation[, .(n = .N, mn = mean(i_intervention_effect), v = var(i_intervention_effect)), c('intervention', 'before_after')]
example_simulation[, .(n = .N, mn = mean(i_final_risk), v = var(i_final_risk)), c('intervention', 'before_after')]

#  basic model (not mixed)
#  .......................

basic_model <- glm(outcome ~ before_after*intervention, data = example_simulation, family = 'poisson')
as.matrix(exp(basic_model$coefficients))

# (Intercept)                         0.1680000 # baseline risk
# before_afterafter                   0.8785714 # secular time trend
# interventioniHost                   1.1150794 # baseline difference, iHost vs. comparison sites
# before_afterafter:interventioniHost 0.7574574 # intervention effect

#  mixed model (random intercept for hospital site)
#  ................................................

mixed_model <- glmer(outcome ~ before_after*intervention + (1|site), data = example_simulation, family = 'poisson', nAGQ = 0)
as.matrix(exp(fixef(mixed_model)))

# (Intercept)                         0.1674099
# before_afterafter                   0.8785714
# interventioniHost                   1.0903631
# before_afterafter:interventioniHost 0.7574574


#  plot iHOST effect
#  .................

nd <- expand.grid(before_after = c('before', 'after'), intervention = c('iHost', 'comparison'))
nd$dama <- predict(basic_model, newdata = nd, type = 'response')
iHost_counter <- exp(sum(coef(basic_model)[1:3]))
iHost_counter <- c(with(nd, dama[before_after == 'before' & intervention == 'iHost']), iHost_counter)

cols <- brewer.pal(3, 'Set1')

#emf('risk_figure.emf', height = 6, width = 6, family = 'Verdana')
png('risk_figure.png', height = 6, width = 6, units = 'in', res = 300)

par(xpd = NA)
plot(1, type = 'n', xlim = c(0.5, 3.5), ylim = c(0.12, 0.2), axes = F, xlab = NA, ylab = NA)
rect(0.5, 0.12, 3.5, 0.2)
with(nd[nd$intervention == 'iHost',], lines(x = 1:2, y = dama, type = 'b', pch = 19, col = cols[1]))
with(nd[nd$intervention == 'comparison',], lines(x = 1:2, y = dama, type = 'b', pch = 19, col = cols[2]))
lines(1:2, iHost_counter, type = 'b', pch = 19, col = cols[1], lty = 3)
text(2.1, c(with(nd, dama[before_after == 'after']), iHost_counter[2]), c('Observed iHost', 'Observed control', 'Counterfactual iHOST'), adj = 0, col = cols[c(1,2,1)])
text(1:2, 0.115, c('Before\niHost', 'During\niHost'))
axis(2, seq(0.13, 0.2, 0.01), pos = 0.5, las = 2)
title(ylab = 'Risk of discharge against medical advice')

dev.off()

#  :::::::::::::::::::::
#  do power calculation
#  ....................

#  using standard model
#  ....................

msim <- function (nsims = 10, iHost_effect, printInt = 100, ...) {
  t(sapply(seq_len(nsims), function (x) { # prints off the number of the simulation
    if (x %% printInt == 0) print(paste0('effect = ', iHost_effect, '; ', 'sim = ', x))
    d <- sim(iHost_effect = iHost_effect, ...)
    m <- glm(outcome ~ before_after*intervention, data = d, family = 'poisson')
    summary(m)$coef[4, c(1, 4)]
  }))
}

# do simulations for different effect sizes

effectsToTest <- seq(0.2, 1, 0.05)
t0 <- proc.time()
set.seed(44)
power1 <- mapply(msim, nsims = 1000, iHost_effect = effectsToTest, SIMPLIFY = F)
power2 <- mapply(msim, nsims = 1000, iHost_effect = effectsToTest, number_iHost_sites = 1, SIMPLIFY = F)
t1 <- proc.time()
t1 - t0

# plot power

xCurve <- seq(0.2, 1, 0.01)

power1_values <- sapply(power1, function (x) mean(x[, 2] < alpha))
power1_model <- glm(power1_values ~ effectsToTest, family = 'binomial')
power1_curve <- predict(power1_model, newdata = data.frame(effectsToTest = xCurve), type = 'response')
power1_80 <- xCurve[which.min(abs(power1_curve - 0.8))]

power2_values <- sapply(power2, function (x) mean(x[, 2] < alpha))
power2_model <- glm(power2_values ~ effectsToTest, family = 'binomial')
power2_curve <- predict(power2_model, newdata = data.frame(effectsToTest = xCurve), type = 'response')
power2_80 <- xCurve[which.min(abs(power2_curve - 0.8))]

#emf('power_figure.emf', height = 6, width = 6, family = 'Verdana')
png('power_figure.png', height = 6, width = 6, units = 'in', res = 300)

plot(1, type = 'n', xlim = c(0, 1), ylim = c(0, 1), xlab = NA, ylab = NA, axes = F)
rect(0, 0, 1, 1)
axis(1, 0:5/5, pos = 0)
axis(2, 0:5/5, pos = 0, las = 2)

points(effectsToTest, power1_values, col = cols[1])
lines(xCurve, power1_curve, col = cols[1])
segments(power1_80, 0.1, y1 = 0.8, lty = 3, col = cols[1])
segments(0, 0.8, x1 = power1_80, lty = 3, col = cols[1])
text(power1_80, 0.01, power1_80, srt = 90, col = cols[1], adj = 0)

points(effectsToTest, power2_values, col = cols[2])
lines(xCurve, power2_curve, col = cols[2])
segments(power2_80, 0.1, y1 = 0.8, lty = 3, col = cols[2])
segments(0, 0.8, x1 = power2_80, lty = 3, col = cols[2])
text(power2_80, 0.01, power2_80, srt = 90, col = cols[2], adj = 0)

title(ylab = 'Power at 95% significance')
title(xlab = 'iHOST effect (risk ratio)')

dev.off()

#  mixed model (takes longer)
#  ..........................

msimMixed <- function (nsims = 10, iHost_effect, printInt = 100, ...) {
  t(sapply(seq_len(nsims), function (x) { # prints off the number of the simulation
    if (x %% printInt == 0) print(paste0('effect = ', iHost_effect, '; ', 'sim = ', x))
    d <- sim(iHost_effect = iHost_effect, ...)
    m <- glmer(outcome ~ before_after*intervention + (1|site), data = d, family = 'poisson', nAGQ = 0)
    summary(m)$coef[4,c(1, 4)]
  }))
}

set.seed(11)
power1mixed <- msimMixed(nsims = 1000, iHost_effect = power1_80, printInt = 100)
mean(power1mixed[,2] < alpha) # power = 0.772 (set at 0.8)
