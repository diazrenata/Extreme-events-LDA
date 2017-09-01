library(portalr)
library(dplyr)



create_rodent_table <- function(period_first, period_last, selected_treatment){
  rodents = abundance(path = 'repo', level = 'Treatment', type = 'Rodents', length = 'longterm', unknowns= F, incomplete = F, shape = 'flat', time = 'period')
  
  # question - how many plots were trapped per treatment per period?
  
  
  
  portalData = loadData('repo')
  rodent_data = portalData[[1]]
  species_table = portalData[[2]]
  trapping_table = portalData[[3]]
  newmoons_table = portalData[[4]]
  plots_table = portalData[[5]]
  
  
  rodent_data = remove_suspect_entries(rodent_data)
  rodent_data = process_unknownsp(rodent_data, species_table, F)
  
  rodent_data = join_plots_to_rodents(rodent_data, plots_table)
  
  trapping_table = filter_plots(trapping_table, 'longterm')
  rodent_data = join_trapping_to_rodents(rodent_data, trapping_table, F)
  
  
  allplots <- rodent_data[,c('plot', 'period', 'treatment', 'sampled')]
  allplots <- unique(allplots)
  
  counted.plots = aggregate(allplots$sampled, by = list(treatment = allplots$treatment, period = allplots$period), FUN = 'sum')
  colnames(counted.plots) <- c('treatment', 'period','plots.trapped')
  
  species <- as.vector(unique(species_table[ which(species_table$taxa == 'Rodent'), 'species']))
  rodents = counted.plots
  rodents[,species] <- NA
  rodents$othercount <- NA
  for (i in 1:nrow(rodents)) {
    for (j in 1:length(species)) {
      this <- filter(rodent_data, period == rodents$period[i] & treatment == rodents$treatment[i])
      rodents[i,species[j]] <- nrow(this[which(this$species == species[j]), ])
      rodents$othercount[i] <- length(unique(this$plot))
    }
  }
  
  r.abund = abundance('repo', level = 'Treatment', type = 'Rodents', length = 'longterm', unknowns = F, incomplete = F, shape = 'list')
  
  # rodents should be the same as r.abund
  
  
  rodents.ex <- filter(rodents, treatment == selected_treatment)
  
  
  # select time period following erica
  period_first = 1
  period_last = 436
  rodents.ex <- filter(rodents.ex, period >=period_first & period<= period_last)
  
  # round incomplete censuses following erica
  rodents_ex_adjusted = as.data.frame.matrix(rodents.ex)
  for (n in 1:nrow(rodents_ex_adjusted)) {
    #divide by number of  plots actually trapped (should be 4) and multiply by 4 to estimate captures as if all plots were trapped
    rodents_ex_adjusted[n,4:30] = round((rodents_ex_adjusted[n,4:30]/rodents_ex_adjusted[n,3])*4)
  }
  
  rodents_ex_adjusted <- rodents_ex_adjusted[, c('BA','DM','DO','DS','NA','OL','OT','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO','SF','SH','SO')]
  
  return(rodents_ex_adjusted)
}

# rodents <- create_rodent_table(1, 436, 'exclosure')
