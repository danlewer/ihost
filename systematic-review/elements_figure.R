library(RColorBrewer)

# read data

d <- read.csv("https://raw.githubusercontent.com/danlewer/ihost/main/systematic-review/elements.csv")

# matrix of studies vs elements

elements <- c('Guidance or protocol',
              'Continuity of OAT from community to hospital',
              'Continuity of OAT from hospital to community',
              'Training and education',
              'Peer support',
              'Multidisciplinary care',
              'Electronic workflow')
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

# calculate elements by intervention type

d$type <- factor(d$type, c('ed_ost', 'addiction_consult', 'multidisciplinary_care', 'skills_procedures'))
type <- split.data.frame(mat, f = d$type)
type <- sapply(type, colSums)
type <- cbind(type, total = colSums(mat))
type[type == 0] <- NA_integer_

element_labels <- c("Guidance or\nprotocol",
                    "Continuity of OAT\nfrom community to\nhospital",
                    "Continuity of OAT\nfrom hospital to\ncommunity",
                    "Training and\neducation in\nrelation to OST",
                    "Peer support\nfor patients",
                    "Multidisciplinary\nworking to\nimprove OST",
                    "Electronic\nworkflow")

cols <- brewer.pal(5, 'Pastel1')

png('element_type_fig.png', height = 7, width = 10, units = 'in', res = 300)

par(xpd = NA, mar = c(4, 6, 0, 0))
plot(1, type = 'n', xlim = c(0, 5.5), ylim = c(0, 7.5), axes = F, xlab = NA, ylab = NA)
segments(1:5, y0 = 0.5, y1 = 7.5)
segments(0.5, 1:7, x1 = 5.5)
symbols(x = rep(1:5, 7), y = rep(1:7, each = 5), circles = sqrt(c(t(type)))/17, inches = F, bg = rep(cols, 7), add = T)
text(x = rep(1:5, 7), y = rep(1:7, each = 5), labels = c(t(type)))
text(1:5, 0.25, c('1. OST initiated in\nEmergency\nDepartments\n(n=17)', '2. Addiction\nconsult teams\n(n=13)', '3. Multidisciplinary\nclinical care\n(n=3)', '4. Hospital-wide\nskills and\nprocedures\n(n=8)', 'All\ninterventions\n(n=41)'), adj = c(0.5, 1))
text(0.25, 1:7, element_labels, adj = 1)
text(3, -1, 'Intervention type', font = 2)
text(-1, 4, 'Intervention element', srt = 90, font = 2)

dev.off()
