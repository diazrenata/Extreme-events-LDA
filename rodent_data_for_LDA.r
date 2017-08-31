library(RCurl)
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
  
  # retrieve current version of rodent data
  rodents = read.csv(text=getURL("https://raw.githubuserßcontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
                    na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
  
  # # retreive current Portal datafiles from GitHub
  current = loadData('repo')
  rodents = current[[1]]
  traps = current[[3]]
  plots = current[[5]]
  # 
  # 
  # # or
  rod = abundance(path = 'repo', level = 'Treatment', type = 'Rodents', length = 'All', unknowns = F, incomplete =F, shape = 'flat', time = 'period')
  # 
  # extract desired data by period, plot, and species
  
  ex.rod = filter(rod, period>=period_first, period<=period_last,
               # plot %in% selected_plots,
               treatment == 'exclosure',
               species %in% selected_species)
  
  # create table of species counts by period
  r_table = table(ex.rod$period,ex.rod$species)
  
  # retrieve data on number of plots trapped per month
  trap_table = read.csv('https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv')
  trap_table_controls = filter(trap_table, plot %in% selected_plots)
  nplots_controls = aggregate(trap_table_controls$sampled,by=list(period = trap_table_controls$period),FUN=sum)
  
  # adjust species counts by number of plots trapped that month
  r_table_adjusted = as.data.frame.matrix(r_table)
  for (n in 1:436) {
    #divide by number of control plots actually trapped (should be 8) and multiply by 8 to estimate captures as if all plots were trapped
    r_table_adjusted[n,] = round(r_table_adjusted[n,]/nplots_controls$x[n]*8)
  }
  
  #write.table(r_table_adjusted,'Rodent_table_dat.csv',sep=',',row.names = F)
  return(r_table_adjusted)
}

