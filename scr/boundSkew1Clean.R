# clean boundary skew 1 data set
# KLS 1.11.19

# import libraries and set working directory
rm(list=ls())
setwd('~/R_Projects/boundSkew1/data')
library(reshape2)

# import data
d0 <- read.csv('Skew_boundary_raw_numeric.csv', na.strings = c('', 'NA'))

# remove first two rows (not data)
d0 <- d0[3:nrow(d0),]

# limit skew trials for now
d1 <- d0[c(grep('ResponseId', colnames(d0)), grep('Age', colnames(d0)), grep('CT2', colnames(d0)):grep('CT1', colnames(d0)), grep('0_95_5', colnames(d0)):grep('n5_55_45', colnames(d0)), grep('Q134', colnames(d0)))]

# remove incomplete records
d1 <- d1[complete.cases(d1),]

# screen data based on response to catch trials - remove participants who do not answer consistently
d3 <- d1[which(d1$CT1 == 1),]
d4 <- d3[which(d3$CT2 == 2),]
rm(d0,d1,d3)

# remove catch trials
d4$CT2 <- NULL
d4$CT1 <- NULL

# recode age into actual age
d4$Age <- as.integer(as.character(d4$Age)) + 19

# make data long
d2 <- melt(d4, id.vars = c('ResponseId', 'Age'), variable.name = 'gamble')
rm(d4)

# recode value into acceptance - 0 (for reject) and 1 (for accept)
d2$accept <- as.numeric(d2$value) - 1

# add condition names 
# valence (gain/loss/neutral)
d2$v1 <- as.factor(t(as.data.frame(strsplit(as.character(d2$gamble), '_')))[,1])
d2$valence[d2$v1 == 'X0'] <- 'neutral'
d2$valence[d2$v1 == 'X0.5'] <- 'gain'
d2$valence[d2$v1 == 'X5'] <- 'gain'
d2$valence[d2$v1 == 'n0.5'] <- 'loss'
d2$valence[d2$v1 == 'n5'] <- 'loss'

# magnitude (0, 0.5, 5)
d2$magnitude[d2$v1 == 'X0'] <- 0
d2$magnitude[d2$v1 == 'X0.5'] <- 0.5
d2$magnitude[d2$v1 == 'X5'] <- 5
d2$magnitude[d2$v1 == 'n0.5'] <- 0.5
d2$magnitude[d2$v1 == 'n5'] <- 5
d2$v1 <- NULL

# deg of skew (low, medium, high)
d2$v2 <- as.factor(t(as.data.frame(strsplit(as.character(d2$gamble), '_')))[,2])

d2$deg_skew[d2$v2 == 95 | d2$v2 == 5] <- 'Strong'
d2$deg_skew[d2$v2 == 90 | d2$v2 == 10] <- 'Strong'
d2$deg_skew[d2$v2 == 85 | d2$v2 == 15] <- 'Strong'
d2$deg_skew[d2$v2 == 80 | d2$v2 == 20] <- 'Moderate'
d2$deg_skew[d2$v2 == 75 | d2$v2 == 25] <- 'Moderate'
d2$deg_skew[d2$v2 == 70 | d2$v2 == 30] <- 'Moderate'
d2$deg_skew[d2$v2 == 65 | d2$v2 == 35] <- 'Weak'
d2$deg_skew[d2$v2 == 60 | d2$v2 == 40] <- 'Weak'
d2$deg_skew[d2$v2 == 55 | d2$v2 == 45] <- 'Weak'
d2$deg_skew[d2$v2 == 50 | d2$v2 == 50] <- 'Symmetric'

# dir of skew (positive or negative)
d2$v2 <- as.numeric(as.character(d2$v2))
d2$dir_skew[d2$v2 > 50] <- 'Negative'
d2$dir_skew[d2$v2 < 50] <- 'Positive'
d2$dir_skew[d2$v2 == 50] <- 'Symmetric'
d2$v2 <- NULL


#save data frame
write.csv(d2, 'boundSkew1Long.csv', row.names = FALSE)
