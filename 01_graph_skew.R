# graph skew data
# 6.10.19 KLS updated 12.2.19

# load required packages
library(here)
library(ggplot2)
library(gdata)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'SummarySE.R'))
source(here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),grep('X0', colnames(dt))[1]:grep('CATCH2.4', colnames(dt))) # no symmetric trials here
d1 <- clean_skew(d0)

# recode response to acceptance
d1$accept <- d1$response - 1

# reorder deg_skew factor
d1$deg_skew <- factor(d1$deg_skew, levels = c('Weak', 'Moderate', 'Strong')) # no symmetric here

# make magnitude a factor
d1$magnitude <- factor(d1$magnitude)

# emulate color palette from study 1
colors <- c('#7CAE00', '#00BFC4', '#C77CFF')

# age effects on acceptance rates
ggplot(d1, aes(Age, accept, colour = deg_skew, fill = deg_skew)) + geom_smooth(method=lm) +
  theme(legend.position = 'top') + facet_grid(magnitude ~ valence) + 
  scale_fill_manual(values = colors) + scale_colour_manual(values=colors)

# create summary - main effect of degree of skewness
d2 <- summarySE(data=d1, measurevar = 'accept', groupvars='deg_skew')

p1 <- ggplot(d2, aes(deg_skew, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Acceptance Rate') + expand_limits(y=c(-.06, 1)) + 
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), legend.justification = c(1,1), legend.position=c(1,1)) + #xlab('Degree of Skewness')
  scale_fill_manual(values = colors, name = 'Skewness') + scale_colour_manual(values=colors, name = 'Skewness')

# create summary - add interaction with valence of gamble
d3 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence','deg_skew'))

p4 <- ggplot(d3, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + scale_fill_manual(values = colors) + scale_colour_manual(values=colors)

# create summary - add interaction with magnitude of gamble
d4 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('magnitude','deg_skew'))

p5 <- ggplot(d4, aes(magnitude, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + scale_fill_manual(values = colors) + scale_colour_manual(values=colors)

# create summary - 3 way interaction 
d5 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('valence', 'magnitude', 'deg_skew'))

ggplot(d5, aes(valence, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  theme(legend.position = 'top') + facet_wrap(~ magnitude) + 
  scale_fill_manual(values = colors) + scale_colour_manual(values=colors)

# create summary - interaction between magnitude and valence 

# make interaction term
d1$magval <- interaction(d1$valence, d1$magnitude)
d1$magval <- drop.levels(d1$magval)
d1$magval <- factor(d1$magval, levels = c('loss.5', 'loss.0.5', 'neutral.0', 'gain.0.5', 'gain.5'))
levels(d1$magval)[1] <- '-$5.00'
levels(d1$magval)[2] <- '-$0.50'
levels(d1$magval)[3] <- '$0.00'
levels(d1$magval)[4] <- '+$0.50'
levels(d1$magval)[5] <- '+$5.00'

d6 <- summarySE(data=d1, measurevar = 'accept', groupvars=c('magval', 'deg_skew'))

p2 <- ggplot(d6, aes(magval, accept, fill = deg_skew)) + geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=accept - se, ymax = accept + se), width = .2, position=position_dodge(.9)) + 
  scale_fill_discrete(name = 'Degree of Skewness') + xlab('Valence by Magnitude Interaction') +
  theme_minimal() +theme(legend.position = 'none', axis.title.y = element_blank()) + expand_limits(y=1) + 
  scale_fill_manual(values = colors) + scale_colour_manual(values=colors)

p3 <- multiplot(p1, p2, layout = matrix(c(1,2,2,2), nrow=1, byrow=TRUE))

#ggsave("deg_skew_plot2.pdf", plot = p3, device="pdf", path="figs/")

pdf('figs/deg_skew_plot2.pdf', width = 12, height = 6)
multiplot(p1, p2, layout = matrix(c(1,2,2,2), nrow=1, byrow=TRUE))
dev.off()

