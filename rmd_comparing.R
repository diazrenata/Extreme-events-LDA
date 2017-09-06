# Comparing erica's method to what you can do with abundance()
library(portalr)
library(dplyr)

# ERICA's table is saved as from_ericas.csv

# RENATA using portalr
# 1. get current rodent data
rod.abundance <- abundance(path = 'repo', level = 'Plot', type = 'Rodents', length = 'all', unknowns = F, incomplete = TRUE, shape = 'flat', time = 'period')

# 2. filter to desired species, time period, and treatment
# should already be filtered to species
# compare these species to erica's species
portalr.species <- as.character(unique(rod.abundance$species))
erica.species <- c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO')
compare.species <- portalr.species == erica.species
which(compare.species == FALSE)
# all the same
rm(compare.species)
rm(erica.species)
rm(portalr.species)

# filtering to control plots does not give the same set of plots as erica used.
# what if you use long-term control plots?
# nor does filtering to long-term control plots
rod.abundance <- abundance(path = 'repo', level = 'Plot', type = 'Rodents', length = 'longterm', unknowns = F, incomplete = TRUE, shape = 'flat', time = 'period')

# filtering to time period:
## not sure how erica chose hers
## for the sake of comparison, selecting the same time period
rod.abundance <- filter(rod.abundance, period >= 1, period <= 436)

# getting *treatments* per *time period*

portalData = loadData('repo')
rodent_data = portalData[[1]]
species_table = portalData[[2]]
trapping_table = portalData[[3]]
newmoons_table = portalData[[4]]
plots_table = portalData[[5]]

# join trapping_table to plots_table?
#head(trapping_table)
#head(plots_table)

plots_trapped_treatment <- join(trapping_table, plots_table, by = c('year', 'month', 'plot'), type = 'left')

#head(rod.abundance)

# just to tell how many plots there should be per period
#rod.abundance.plots <- rod.abundance[,c('period', 'plot')]
#rod.abundance.plots <- unique(rod.abundance.plots)

#plots_trapped_treatment_positive <- plots_trapped_treatment[ which(plots_trapped_treatment$sampled == 1), c('period', 'plot', 'treatment')]
# this has more rows than rod.abundance
# i think this is bc rod.abundance is filtered to time period:

#max(rod.abundance.plots$period) # 436
#max(plots_trapped_treatment_positive$period) # 463
#plots_trapped_treatment_positive <- filter(plots_trapped_treatment_positive, period >=1, period <=436)
# well now plots has fewer rows. mb rod.abundance has plots that werent trapped?

#plots_trapped_treatment <- filter(plots_trapped_treatment, period >= 1, period <= 436)
# yay! these have the same # of rows

#rm(plots_trapped_treatment_positive)

# ok so join rod.abundance with plots_trapped_treatment
# remove some columns from plots_trapped_treatment first

rod.abundance <- join(rod.abundance, plots_trapped_treatment, by = c('period', 'plot'), type = 'left')

# to recap, to get this you need to
## join plots_table to trapping_table by c('year', 'month', 'plot)
## join the above to rodent_abundance (by plot) by c('period', 'plot')
## gives table:
#head(rod.abundance)
# period plot species abundance day month year sampled treatment
# 1      1    1      BA         0  16     7 1977       1   control
# 2      1    1      DM         2  16     7 1977       1   control
# 3      1    1      DO         0  16     7 1977       1   control
# 4      1    1      DS         0  16     7 1977       1   control
# 5      1    1      NA         0  16     7 1977       1   control
# 6      1    1      OL         0  16     7 1977       1   control


## do controls for comparison
# the above you can summarize by treatment and time period

rod.abundance.controls <- filter(rod.abundance, treatment == 'control')

# do these plots line up?
rod.abundance.controls.plots <- unique(rod.abundance.controls$plot)
erica.plots <- c(2,4,8,11,12,14,17,22)
rod.abundance.controls.plots
#### INCOMPLETE MATCH #### so I don't know how to recreate erica's list of plots using portalr 

# per period you want abundance of each species, and how many plots were trapped that session
rod.table.control <- matrix(data=NA, nrow = length(unique(rod.abundance.controls$period)), ncol = 3 + as.numeric(length(unique(rod.abundance.controls$species))))
colnames(rod.table.control) <- c('period', 'nsampled', 'treatment', as.vector(unique(rod.abundance.controls$species)))
rod.table.control[, 'period'] <- as.vector(unique(rod.abundance.controls$period))
head(rod.table.control)

for (p in 1:nrow(rod.table.control)) {
  period <- filter(rod.abundance.controls, period == p)
  
  splist <- as.vector(unique(period$species))
  
  for(sp in splist) {
    period.sp <- filter(period, species == sp)
    
    sp.sum <- as.numeric(sum(period.sp$abundance, na.rm = TRUE))
    
    rod.table.control[p,sp] <- sp.sum
    
  }
  
  period.plots <- filter(period, sampled == 1)
  nplots <- as.numeric(length(unique(period.plots$plot)))
  
  rod.table.control[p, 'nsampled'] <- nplots
  
  rod.table.control[p, 'treatment'] <- as.character(period[1, 'treatment'])
  
}

## this gives probably different results from erica
## subsetting to erica's plots

erica.plots <- c(2,4,8,11,12,14,17,22)

rod.abundance.erica <- filter(rod.abundance, plot %in% erica.plots)

rod.table.erica <-  matrix(data=NA, nrow = length(unique(rod.abundance.erica$period)), ncol = 3 + as.numeric(length(unique(rod.abundance.erica$species))))
colnames(rod.table.erica) <- c('period', 'nsampled', 'treatment', as.vector(unique(rod.abundance.erica$species)))
rod.table.erica[, 'period'] <- as.vector(unique(rod.abundance.erica$period))
head(rod.table.erica)

for (p in 1:nrow(rod.table.erica)) {
  period <- filter(rod.abundance.erica, period == p)
  
  splist <- as.vector(unique(period$species))
  
  for(sp in splist) {
    period.sp <- filter(period, species == sp)
    
    sp.sum <- as.numeric(sum(period.sp$abundance, na.rm = TRUE))
    
    rod.table.erica[p,sp] <- sp.sum
    
  }
  
  period.plots <- filter(period, sampled == 1)
  nplots <- as.numeric(length(unique(period.plots$plot)))
  
  rod.table.erica[p, 'nsampled'] <- nplots
  
  rod.table.erica[p, 'treatment'] <- as.character(period[1, 'treatment'])
  
}

# adjust

rod.table.erica.adj <- rod.table.erica

for (n in 1:nrow(rod.table.erica.adj)) {
  
  for (i in 4:24){
  rod.table.erica.adj[n, i] <- round(as.numeric(rod.table.erica.adj[n, i])/as.numeric(rod.table.erica.adj[n, 'nsampled']) * 8)
  }
}


rod.table.erica.adj <- rod.table.erica.adj[,4:24]
rod.table.erica.adj <- as.data.frame(rod.table.erica.adj)

compare <- rod.table.erica.adj == from.ericas
which(compare == FALSE)

## ^ successfully recreated erica's data. 

## do long-term exclosures for LDA

## species = rodents, no X's.
## time period = longterm or not 
## treatment = exclosure, control, or removal
# 3. figure out how many plots were trapped per period
# 4. adjust rodent abundances by n plots

