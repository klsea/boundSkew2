# score skew data
# 6.10.19 KLS

# load required packages
library(here)
library(ggplot2)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew1_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(9,12),20:79)
d1 <- clean_skew(d0)

# recode response to acceptance
d1$accept <- d1$response - 1

# reorder deg_skew factor
d1$deg_skew <- factor(d1$deg_skew, levels = c('Symmetric', 'Weak', 'Moderate', 'Strong'))

# make magnitude a factor
d1$magnitude <- factor(d1$magnitude)

# age effects on acceptance rates
ggplot(d1, aes(Age, accept, colour = deg_skew, fill = deg_skew)) + geom_smooth(method=lm) +
  theme(legend.position = 'top') + facet_grid(magnitude ~ valence)

# create summary - main effect of degree of skewness
d2 <- summarySE(data=d1, measurevar = 'accept', groupvars='deg_skew')

ggplot(d2, aes(deg_skew, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'none')

# create summary - add interaction with valence of gamble
d3 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence','deg_skew'))

ggplot(d3, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')

# create summary - add interaction with magnitude of gamble
d4 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('magnitude','deg_skew'))

ggplot(d4, aes(magnitude, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top')

# create summary - 3 way interaction 
d5 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence', 'magnitude', 'deg_skew'))

ggplot(d5, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + facet_wrap(~ magnitude)
