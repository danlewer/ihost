library(RColorBrewer)

# read data

d <- read.csv("https://raw.githubusercontent.com/danlewer/ihost/main/systematic-review/elements.csv")
d <- d[d$include == 'yes' & d$country != '',]
d$country <- factor(d$country, c('United States', 'Canada', 'United Kingdom'))
d <- d[order(d$country, -d$study_year),]
d$country <- as.character(d$country)

# y positions

d <- d[nrow(d):1,]
d$y <- 1:nrow(d)
d$y <- d$y + cumsum(as.integer(d$country != c('United Kingdom', d$country[-length(d$country)])) * 2)

# matrix of studies vs elements

elements <- c('Guidance or protocol',
              'Continuity of OAT from community to hospital',
              'Continuity of OAT from hospital to community',
              'Assessing eligibility',
              'Training and education',
              'Peer support',
              'Specialist liaison and addiction consult teams')
mat <- sapply(elements, function (x) grepl(x, d$elements))

# check elements

spl <- lapply(d$elements, strsplit, split = ';', fixed = T)
spl <- lapply(spl, unlist)
spl <- lapply(spl, function (x) x %in% elements)
sapply(spl, all) # should all be true

# analysis of elements

cbind(colSums(mat), colSums(mat) / nrow(mat) * 100) 
table(rowSums(mat)) # most include one element
table(apply(mat * 1, 1, paste, collapse = ''))

# matrix of colour codes for medication types

cols <- brewer.pal(4, 'Pastel1')
mat2 <- rep('white', nrow(d))
mat2[d$ost_medication_type != ''] <- cols[4]
mat2[d$ost_medication_type == 'Methadone'] <- cols[1]
mat2[d$ost_medication_type == 'Buprenorphine'] <- cols[2]
mat2[d$ost_medication_type == 'Methadone and buprenorphine'] <- cols[3]
mat2 <- do.call(cbind, rep(list(mat2), 7))
mat2[!mat] <- 'white'

# figure

xl <- rep(1:7, nrow(d))
yl <- rep(d$y, each = 7)
d$id <- paste0(d$first_author, ' ', d$study_year)
types2 <- c('Guidance or\nprotocol',
            'Continuity of OAT\nfrom community\nto hospital',
            'Continuity of OAT\nfrom hospital\nto community',
            'Assessing\neligibility',
            'Training and\neducation',
            'Peer\nsupport',
            'Specialist liaison\nand addiction consult\nteams')
ys <- seq(max(d$y) * 0.8, max(d$y) + 1, length.out = 5)

png('ihost_sys_rvw_matrix2.png', height = 12, width = 9, units = 'in', res = 300)

par(xpd = NA, mar = c(12, 7, 1, 8))
plot(1, type = 'n', xlim = c(1, 8), ylim = c(3, 48), axes = F, xlab = NA, ylab = NA)
rect(xl, yl, xl + 1, yl + 1, col = t(mat2))
text(rep(1, 4), y = c(2, 9, 48) + 0.5, c('United Kingdom', 'Canada', 'United States'), adj = c(0, 0), font = 2)
text(1:7 + 0.5, 0, types2, srt = 90, adj = 1)
text(0.9, d$y + 0.5, d$id, adj = 1)
rect(8.25, ys[-length(ys)], 8.5, ys[-1], col = rev(cols))
text(8.6, ys[-length(ys)] + diff(ys)/2, rev(c('Methadone', 'Buprenorphine', 'Methadone and\nbuprenorphine', 'Other')), adj = 0)
text(8.25, 48.5, 'Type of medication', adj = c(0, 0), font = 2)

dev.off()
