library(dplyr)
library(portalr)

#' Create rodent species table
#'
#' Processes rodent capture data so it can be used for LDA analysis
#' 
#'
#' @param period_first first trapping period desired
#' @param period_last last trapping period desired
#' @param selected_plots vector of plot numbers to be included
#' @param selected_species vector of species codes to be included
#' 
#' @return Table of species counts per period
#' 
#' @example
 r_table = create_rodent_table(1,436,c(2,4,8,11,12,14,17,22),
                              selected_species = c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO'))
#'

create_rodent_table = function(period_first,period_last,selected_plots,selected_species) {
  
  
  # # retreive current Portal datafiles from GitHub
  current = loadData('repo')
  rodents = current[[1]]
  traps = current[[3]]
  plots = current[[5]]
  # 
  # 
  # # or
  rod = abundance(path = 'repo', level = 'Plot', type = 'Rodents', length = 'All', unknowns = F, incomplete =F, shape = 'flat', time = 'period')
  # 
  # extract desired data by period, plot, and species
  
  ex.rod = filter(rod, period>=period_first, period<=period_last,
                  # plot %in% selected_plots,
                  treatment == 'exclosure',
                  species %in% selected_species)
  
  # create table of species counts by period
  r_table = table(ex.rod$period,ex.rod$species)
  
  ##### Erica
  
  period_first <- 1
  period_last <- 436
  selected_plots <- c(2,4,8,11,12,14,17,22)
  selected_species <- c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO')
  
  
  
  rodents.e = read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
                     na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
  # 
  # ### using portalr
  
  ?abundance
  
  rodentsByPeriod <- abundance(path = 'repo', level = 'Treatment', type = 'Rodents', length = 'Longterm', unknowns = F, incomplete = TRUE, shape = "crosstab", time  = "period")
  
  controlsByPeriod <- rodents[ which(rodents$treatment == 'control'), c('period', selected_species)]
  

  rodentsBySite <-  abundance(path = 'repo', level = 'Site', type = 'Rodents', length = 'Longterm', unknowns = F, incomplete = TRUE, shape = "crosstab", time  = "period")

  
  # replicating erica's dataset
  rodentsByPlot <-  abundance(path = 'repo', level = 'Plot', type = 'Rodents', length = 'All', unknowns = F, incomplete = TRUE, shape = "crosstab", time  = "period")
  
  rodentsByPlot <- filter(rodentsByPlot, plot %in% selected_plots)
  
  incompletes <-  rodentsByPlot[ which(is.na(rodentsByPlot$BA)), c('period', 'plot')]

  incompletes <- table(incompletes)
  
  incompleteCount <- vector(length = nrow(incompletes))

  for (i in 1:nrow(incompletes)) {
    incompleteCount[i] <- sum(incompletes[i, 1:4])
  }
  
  incompleteCount <- as.data.frame(incompleteCount)
  incompleteCount$period <- row.names(incompletes)

  rodentAbund <- matrix(data = NA, nrow = length(unique(rodentsByPlot$period)), ncol = length(selected_species), dimnames = list(unique(rodentsByPlot$period), selected_species))
  
  for (i in 1:nrow(rodentAbund)) {
    for (j in 1:ncol(rodentAbund)) {
      this <- rodentsByPlot[ which(rodentsByPlot$period == row.names(rodentAbund)[i]), selected_species[j]]
      rodentAbund[i,j] <- sum(this[,1], na.rm = TRUE)
    }
  }
  
  rodentAbund.adjusted <- rodentAbund
  for (i in 1:nrow(incompleteCount)) {
    for (j in 1:ncol(rodentAbund)) {
      rodentAbund.adjusted[incompleteCount$period[i], j] <- round((rodentAbund[incompleteCount$period[i], j] / (8 - incompleteCount[i, 1])) * 8)
    }
    
  }
  
  rodentAbund.adjusted <- rodentAbund.adjusted[1:436, ]
  
  test <- rodentAbund.adjusted
  
  test <- as.matrix(r_table_adjusted.e) - rodentAbund.adjusted
  r_table_adjusted.e[351, ]
  r_table.e[351, ]
  rodentAbund[351, ]
  
  
  
  # portalDat <- loadData('repo')
  # 
  # rodents <- portalDat[[1]]
  # 
  # treatments <- portalDat[[5]]
  # trapping <- portalDat[[3]]
  # 
  # trap.periods <- unique(trapping[,c('month', 'year', 'period')])
  # 
  # treatments <- join(treatments, trap.periods, by = c('month', 'year'), type = 'left')
  # 
  # treatments <- treatments[,c('plot', 'treatment')]
  # 
  # control <- unique(treatments[ which(treatments$treatment == 'control'), 'plot'])
  # 
  # treatments <- unique(filter(treatments, plot %in% control)[,c('plot', 'treatment')])
  # 
  # unique(treatments)
  # 
  # 
  # rod <- filter(rodents, period >= period_first, period <= period_last, plot %in% selected_plots, species %in% selected_species)
  # 
  # r_table = table(rod$period, rod$species)
  # 
  # 
  # 
  # extract desired data by period, plot, and species
  rod.e = filter(rodents.e, period>=period_first, period<=period_last,
               plot %in% selected_plots,
               species %in% selected_species)
  
  # create table of species counts by period
  r_table.e = table(rod.e$period,rod.e$species)
  

  # retrieve data on number of plots trapped per month
  trap_table.e = read.csv('https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv')
  trap_table_controls.e = filter(trap_table.e, plot %in% selected_plots)
  nplots_controls.e = aggregate(trap_table_controls.e$sampled,by=list(period = trap_table_controls.e$period),FUN=sum)
  
  # adjust species counts by number of plots trapped that month
  r_table_adjusted.e = as.data.frame.matrix(r_table.e)
  for (n in 1:436) {
    #divide by number of control plots actually trapped (should be 8) and multiply by 8 to estimate captures as if all plots were trapped
    r_table_adjusted.e[n,] = round(r_table_adjusted.e[n,]/nplots_controls.e$x[n]*8)
  }
  
  r_table_adjusted.e
}

