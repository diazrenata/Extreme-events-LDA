library(portalr)
library(dplyr)
source('/Users/renatadiaz/Documents/GitHub/portalr/R/biomassAdjustable.R')
# so this only works with these arguments right now
#
# path = 'repo'
# type = 'rodents'
# length = 'longterm'
# unknowns = F
# incomplete = T
# shape = 'list'
# time = 'period'
# dates = T
library(ggplot2)
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra)

biomass.full = biomass.adjustable(path = 'repo', level = 'treatment.adj', type ='rodents', length = 'longterm', unknowns = F, incomplete = T, shape = 'list', 
                             time = 'period', dates = T)


period_first = 1
period_last = 436
selected_treatment = 'exclosure'

r.biomass = biomass.full %>% 
  mutate(biomass = as.numeric(biomass)) %>% 
  filter(period >= period_first, period <= period_last, 
         treatment == selected_treatment) %>% 
  group_by(period, censusdate, n, treatment) %>% 
  summarize(total.biomass = sum(biomass, na.rm = TRUE), total.biomass.perplot = sum(biomass.perplot, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(usual.n = (ceiling(mean(n)))) %>% 
  mutate(total.biomass.adj = round(total.biomass.perplot * usual.n)) %>% 
  mutate(date.axis = as.Date(censusdate))
  
dates = select(r.biomass, censusdate)
dates = as.Date(dates$censusdate)

biomass_thrutime = qplot(x=date.axis, y = total.biomass.adj, data = r.biomass, na.rm = TRUE, main = "Total rodent biomass in exclosures", xlab = "Date", ylab = "Biomass (g)")
biomass_thrutime
