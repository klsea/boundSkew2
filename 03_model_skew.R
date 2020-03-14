# run models on skew data
# 7.15.19 KLS updated 10.17.19

# load required packages
library(here)
library(lme4)
library(gdata)
library(tidyr)
library(rlist)

# load source functions
source(here::here('scr', 'isolate_skew.R'))
source(here::here('scr', 'clean_skew.R'))
source(here::here('scr', 'SummarySE.R'))
source(here::here('scr', 'pairedttable.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here("data", "bound_skew2_data.csv"))

# separate skew
d0 <- isolate_skew(dt, c(1,2), 
                   grep('X0', colnames(dt))[1]:grep('CATCH2.4', colnames(dt))) # no symmetric trials here
d1 <- clean_skew(d0)

# recode response to acceptance
d1$accept <- d1$response - 1

# reorder deg_skew factor
d1$deg_skew <- factor(d1$deg_skew, levels = c('Weak', 'Moderate', 'Strong')) # no symmetric here

# reorder valence factor
d1$valence <- factor(d1$valence, levels = c('neutral', 'gain', 'loss'))

# make magnitude a factor
d1$magnitude <- factor(d1$magnitude, levels = c('0', '0.5', '5'))

# scale and center age
d1$Age <- scale(d1$Age)

# make interaction term
d1$magval <- interaction(d1$valence, d1$magnitude)
d1$magval <- drop.levels(d1$magval)
d1$magval <- factor(d1$magval, levels = c('neutral.0', 'loss.5', 'loss.0.5', 'gain.0.5', 'gain.5'))

# baseline - only degree of skew
b1 <- glmer(accept ~ deg_skew + (1 + Age | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
               control=glmerControl(optimizer='bobyqa'))
summary(b1, correlation = FALSE)

# boundary fit - remove age from random effects
b1.1 <- glmer(accept ~ deg_skew + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(b1.1, correlation = FALSE)
saveRDS(b1.1, here::here('output', 'baseline.RDS'))

##follow-up t-tests
d2 <- summarySE(d1, 'accept', groupvars = c('ID', 'deg_skew'))
d3 <- spread(d2[,c(1,2,4)], 'deg_skew', 'accept')
b1_follow <- pairedttable(d3, colnames(d3[2:4]))
list.save(b1_follow, here::here('output', 'b1_follow.rds'))
rm(d2,d3, b1_follow)

# model 1 - add valence
m1 <- glmer(accept ~ deg_skew * valence + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m1, correlation = FALSE)
saveRDS(m1, here('output', 'm1.RDS'))

# compare model 1 and model 2
anova(b1.1,m1)

# Does magnitude make a difference?

# model 2 -  magnitude instead of valence
m2 <- glmer(accept ~ deg_skew  * magnitude + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(m2, correlation = FALSE)
saveRDS(m2, here::here('output', 'm2.RDS'))

# compare model 2 to baseline
anova(b1.1,m2)

# model 3 - interaction between mag and val
m3 <- glmer(accept ~ deg_skew * magval + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m3, correlation = FALSE)
saveRDS(m3, here::here('output', 'm3.RDS'))

# compare model 3 to baseline
anova(m1, m3)

# model 4 - add Age
m4 <- glmer(accept ~ deg_skew * magval + Age + (1 | ID), data = d1, family = binomial(link = logit), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(m4, correlation = FALSE)
saveRDS(m4, here::here('output', 'm4.RDS'))

# compare model 5 to model 4
anova(m3,m4)

