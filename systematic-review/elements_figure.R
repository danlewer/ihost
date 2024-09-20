library(RColorBrewer)

breakinto <- function (x, lines = 3) {
  y <- strsplit(x, split = ' ', fixed = T)[[1]]
  lines <- min(lines, length(y))
  wpl <- length(y) / lines
  nw <- rep(round(wpl, 0), lines)
  nw[1] <- nw[1] + length(y) - sum(nw)
  nw <- cumsum(nw)
  fw <- c(1, nw + 1)[1:length(nw)]
  i <- mapply(`:`, a = fw, b = nw, SIMPLIFY = F)
  y <- lapply(i, function (x) y[x])
  y <- lapply(y, paste0, collapse = ' ')
  y <- lapply(y, function (x) paste0(x, '\n'))
  y <- do.call(paste0, y)
  substr(y, 1, nchar(y)-1)
}

# read data

d <- read.csv("https://raw.githubusercontent.com/danlewer/ihost/refs/heads/main/systematic-review/elements.csv")
# d$country <- factor(d$country, c('United States', 'Canada', 'United Kingdom'))
# d <- d[order(d$country, -d$study_year),]
# d$country <- as.character(d$country)

# y positions

d <- d[nrow(d):1,]
d$y <- 1:nrow(d)
d$y <- d$y + cumsum(as.integer(d$country != c('United Kingdom', d$country[-length(d$country)])) * 2)

# matrix of studies vs elements

els <- unique(unlist(lapply(d$elements, strsplit, split = ';')))
n_els <- colSums(sapply(els, function (x) grepl(x, d$elements, fixed = T)))
n_els <- n_els[order(n_els, decreasing = T)]
els <- names(n_els)

mat <- sapply(els, function (x) grepl(x, d$elements, fixed = T))

# check elements

spl <- lapply(d$elements, strsplit, split = ';', fixed = T)
spl <- lapply(spl, unlist)
spl <- lapply(spl, function (x) x %in% els)
table(sapply(spl, all)) # should all be true

# analysis of elements

cbind(colSums(mat), colSums(mat) / nrow(mat) * 100) 
table(rowSums(mat)) # most include multiple elements
table(apply(mat * 1, 1, paste, collapse = ''))

# calculate elements by intervention type

d$type <- factor(d$type, c('ed_ost', 'addiction_consult', 'building_capacity'))
type <- split.data.frame(mat, f = d$type)
type <- sapply(type, colSums)
type <- cbind(type, total = colSums(mat))
type[type == 0] <- NA_integer_

cols <- brewer.pal(4, 'Pastel1')
labs <- mapply(breakinto, x = els, lines = round(sapply(els, nchar) / 30, 0))

png('Figure2.png', height = 14, width = 12, units = 'in', res = 300)

par(xpd = NA, mar = c(4, 17, 0, 0))
plot(1, type = 'n', xlim = c(0, 4.5), ylim = c(0, 9.5), axes = F, xlab = NA, ylab = NA)
segments(1:4, y0 = 0.5, y1 = 9.5, lty = 3)
segments(0.5, 1:9, x1 = 4.5, lty = 3)
symbols(x = rep(1:4, 9), y = rep(1:9, each = 4), circles = sqrt(c(t(type)))/17, inches = F, bg = rep(cols, 10), add = T)
text(x = rep(1:4, 9), y = rep(1:9, each = 4), labels = c(t(type)))
text(1:4, 0.25, c('1. Pathways to initiate\nOAT in Emergency\nDepartments\n(n=26)', '2. Addiction\nConsult Teams\n(n=18)', '3. Building capacity\nto provide\nOAT to\ninpatients\n(n=13)', 'All\ninterventions\n(n=57)'), adj = c(0.5, 1))
text(0.25, 1:9, labs, adj = 1)
text(3, -1.3, 'Intervention type', font = 2)
text(-1.5, 4, 'Intervention element', srt = 90, font = 2)

dev.off()
