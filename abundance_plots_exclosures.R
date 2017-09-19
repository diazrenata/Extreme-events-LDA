# scripts for making supporting figures in the extreme events/ LDA project



# ==============================================
# total abundance

dat = read.csv('Rodent_table_dat.csv',na.strings = '',as.is=T)
rdat = read.csv('../PortalData/Rodents/Portal_rodent.csv',as.is=T)
trappingdat = read.csv('../PortalData/Rodents/Portal_rodent_trapping.csv',as.is=T,na.strings = '')
trapdat = aggregate(trappingdat$Sampled,by=list(Period=trappingdat$Period),FUN=sum)
fullcensus = trapdat[trapdat$x>20,]
perioddates = read.csv('../PortalData/Rodents/moon_dates.csv',as.is=T,na.strings = '')
perioddates$CensusDate = as.Date(perioddates$CensusDate)
fullcensus = merge(fullcensus,perioddates)

# data frame
abund_dat = data.frame(Period = 1:436, n = rowSums(dat))
abund_dat = merge(abund_dat,fullcensus[,c('Period','CensusDate')])

abund_dat = read.csv('exclosures_dat.csv')
dates = read.csv('exclosures_dates.csv')
abund_dat = data.frame(abund_dat, n = rowSums(abund_dat))
abund_dat = cbind(abund_dat, dates)

abund_dat$censusdate = as.Date(abund_dat$censusdate)


# plot
plot(abund_dat$censusdate,log(abund_dat$n),xlab='',ylab='Log Total Abundance',pch=19,ylim=c(2,6)) +
rect(xleft = as.Date('1999-07-01'),xright = as.Date('1999-10-01'),ytop = 250,ybottom=0,col='gray',border=NA) + 
rect(xleft = as.Date('1983-08-01'),xright = as.Date('1983-11-01'),ytop = 250,ybottom=0,col='gray',border=NA) + 
rect(xleft = as.Date('1993-09-01'),xright = as.Date('1994-10-01'),ytop = 250,ybottom=0,col='gray',border=NA) +
rect(xleft = as.Date('2009-03-01'),xright = as.Date('2010-01-01'),ytop = 250,ybottom=0,col='gray',border=NA) +
lines(abund_dat$censusdate,log(abund_dat$n)) + 
points(abund_dat$censusdate,log(abund_dat$n),pch=16) + 
abline(h=log(mean(abund_dat$n))) +
box(which='plot')

# line segments showing changepoint 95% intervals
segments(as.Date('1990-05-01'),5.8 , as.Date('1995-03-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1997-02-01'),5.8 , as.Date('1997-11-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('2009-09-01'),5.8 , as.Date('2010-05-01'), 5.8, col='red', lwd=3, xpd = FALSE)

setEPS()
postscript('extreme_events_exclosures.eps')
plot(abund_dat$censusdate,log(abund_dat$n),xlab='',ylab='Log Total Abundance',pch=19,ylim=c(2,6)) 
  rect(xleft = as.Date('1999-07-01'),xright = as.Date('1999-10-01'),ytop = 250,ybottom=0,col='gray',border=NA)  
  rect(xleft = as.Date('1983-08-01'),xright = as.Date('1983-11-01'),ytop = 250,ybottom=0,col='gray',border=NA) 
  rect(xleft = as.Date('1993-09-01'),xright = as.Date('1994-10-01'),ytop = 250,ybottom=0,col='gray',border=NA) 
  rect(xleft = as.Date('2009-03-01'),xright = as.Date('2010-01-01'),ytop = 250,ybottom=0,col='gray',border=NA)
  lines(abund_dat$censusdate,log(abund_dat$n)) 
  points(abund_dat$censusdate,log(abund_dat$n),pch=16) 
  abline(h=log(mean(abund_dat$n))) 
  box(which='plot')

# line segments showing changepoint 95% intervals
segments(as.Date('1990-05-01'),5.8 , as.Date('1995-03-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1997-02-01'),5.8 , as.Date('1997-11-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('2009-09-01'),5.8 , as.Date('2010-05-01'), 5.8, col='red', lwd=3, xpd = FALSE)
dev.off()