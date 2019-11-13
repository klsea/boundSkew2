# graph skew bound 1 data
# 2.4.19 KLS

# import libraries and set working directory
rm(list=ls())
setwd('~/R_Projects/boundSkew1/data')
library(ggplot2)

# load functions
source('~/Dropbox (Personal)/Functions/SummarySE.R')

# load data
dt <- read.csv('boundSkew1Long.csv')

d0 <- na.omit(dt)
# reorder deg_skew factor
d0$deg_skew <- factor(d0$deg_skew, levels = c('Symmetric', 'Weak', 'Moderate', 'Strong'))
# make magnitude a factor
d0$magnitude <- factor(d0$magnitude)

# only look at postive skew
d0 <- d0[which(d0$dir_skew == 'Positive'),]

# age effects on acceptance rates
ggplot(d0, aes(Age, accept, colour = deg_skew, fill = deg_skew)) + geom_smooth(method=lm) +
  theme(legend.position = 'top') + facet_grid(magnitude ~ valence)

# create summary - main effect of degree of skewness
d1 <- summarySE(data=d0, measurevar = 'accept', groupvars='deg_skew')

ggplot(d1, aes(deg_skew, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'none')

# create summary - add interaction with valence of gamble
d2 <- summarySE(data=d0, measurevar = 'accept', groupvars=c('valence','deg_skew'))

ggplot(d2, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')
  
# create summary - add interaction with magnitude of gamble
d3 <- summarySE(data=d0, measurevar = 'accept', groupvars=c('magnitude','deg_skew'))

ggplot(d3, aes(magnitude, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')

# create summary - 3 way interaction 
d4 <- summarySE(data=d0, measurevar = 'accept', groupvars=c('valence', 'magnitude', 'deg_skew'))

ggplot(d4, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + facet_wrap(~ magnitude)


