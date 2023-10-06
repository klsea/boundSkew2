# run exploratory models on age
# 7.26.23 CCF

# load required packages
library(here)
library(lme4)
library(gdata)
library(tidyr)
library(rlist)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

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
b1 <- glmer(accept ~ deg_skew + (1 + Age | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(b1, correlation = FALSE)

# boundary fit - remove age from random effects
b1.1 <- glmer(accept ~ deg_skew + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
              control=glmerControl(optimizer='bobyqa'))
summary(b1.1, correlation = FALSE)
#saveRDS(b1.1, here::here('output', 'baseline.RDS'))


##age exploratory models
#age model 1: just age
am1 <- glmer(accept ~ Age + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
             control=glmerControl(optimizer='bobyqa'))
summary(am1, correlation = FALSE)
tab_model(am1)

#age model 2: age and degree of skewness, no interaction
am2 <- glmer(accept ~ Age + deg_skew + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
            control=glmerControl(optimizer='bobyqa'))
summary(am2, correlation = FALSE)
tab_model(am2)

#age model 3: age and degree of skewness, no interaction
am3 <- glmer(accept ~ Age * deg_skew + (1 | ID), data = d1, family = binomial(link = "logit"), nAGQ = 1, 
             control=glmerControl(optimizer='bobyqa'))
summary(am3, correlation = FALSE)
tab_model(am3)

