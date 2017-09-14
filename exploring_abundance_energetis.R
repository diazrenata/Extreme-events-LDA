
library(topicmodels)
library(RCurl)
# library(multipanelfigure) cannot install this package - issue with 'gridGraphics' (dependency).
library(reshape2)


#source('rodent_data_for_LDA.r')
source('adjusted_abundance.R')
source('AIC_model_selection.R')
source('LDA_figure_scripts.R')
source('changepointmodel.r')
source('LDA-distance.R')


# to skip running the changepoint and just load results
source('readResults.R')

# ===================================================================
# 1. prepare rodent data
# ===================================================================
# dat = create_rodent_table(period_first = 1,
#                           period_last = 436,
#                           selected_plots = c(2,4,8,11,12,14,17,22),
#                           selected_species = c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO'))

dat.full <- adjusted_abundance(period_first = 1,
                               period_last = 436, 
                               selected_treatment = 'exclosure',
                               length = 'longterm',
                               dates = TRUE)



dates <- dat.full[,'censusdates']
dates <- as.Date(dates)
dat <- dat.full[,c(4:24)]
splist <- colnames(dat.full)[4:24]
periods <- dat.full[,'period']

datdims <- dim(dat)

datint <- as.integer(dat)

dat <- matrix(data = datint, nrow = datdims[1], ncol = datdims[2], dimnames = list(periods, splist))


# need non-zero entries?
dat.totals <- vector(length=nrow(dat))
for(i in 1:length(dat.totals)) {
  dat.totals[i] <- sum(dat[i,])
}

which(dat.totals == 0)

from.ericas <- read.csv("from_ericas.csv")
erica.totals <- vector(length = nrow(from.ericas))
for(i in 1:length(erica.totals)) {
  erica.totals[i] <- sum(from.ericas[i, ])
}

which(erica.totals == 0)

#erica had no 0's

# for now, just removing censuses with 0's?

dat.keep <- which(dat.totals != 0)
dates <- dates[dat.keep]
periods <- periods[dat.keep]
dat <- dat[dat.keep, ]


# total abundances v time
total.keep <- dat.totals[dat.keep]
total.time <- as.data.frame(total.keep)
total.time$date <- dates

timeplot <- ggplot(total.time, aes(date, total.keep)) +
  geom_point(na.rm = TRUE)


#### rough biomass through time on exclosure plots ####
#### PAUSE making biomass function ####