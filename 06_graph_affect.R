# graph affective
# 11.11.19 KLS

# load required packages
library(here)
library(ggplot2)
library(tidyr)
library(plyr)

# load source functions
source(here::here('scr', 'isolate_skew.R'))
source(here::here('scr', 'isolate_measure.R'))
source(here::here('scr', 'SummarySE.R'))
source(here::here('scr', 'pairedttable.R'))
source(here::here('scr', 'corrTableCI.R'))
source(here::here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))
dict <- read.csv(here("data", "bound_skew2_data_dictionary.csv"))

# isolate AVI data
Sys.setlocale('LC_ALL','C')
qs <- as.character(dict$Question)
first <- grep("Enthusiastic", qs)
last <- grep("Lonely", qs)
#last <- grep("Serene", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# add labels to AVI data
d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Lonely", dict)
#d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Serene", dict)
d1 <- score_avi(d0)

# Graph means
d2 <- cbind(colMeans(d1), apply(d1, 2, sd), apply(d1, 2, sd)/sqrt(nrow(d1)))
d2 <- d2[3:nrow(d2),]
colnames(d2) <- c('Mean', 'SD', 'SE')
names <- rownames(d2); rownames(d2) <- NULL
d2 <- cbind(names, d2)
d2 <- data.frame(d2)
d2$Mean <- as.numeric(as.character(d2$Mean))
d2$SD <- as.numeric(as.character(d2$SD))
d2$SE <- as.numeric(as.character(d2$SE))
d2$names <- revalue(d2$names, c('ha' = 'high arousal','han' = 'high arousal - negative',
                                'hap' = 'high arousal - positive', 'la' = 'low arousal', 
                                'lan' = 'low arousal - negative', 'lap' = 'low arousal - positive'))
d2$names <- factor(d2$names, levels =c('low arousal', 'low arousal - negative', 'low arousal - positive', 
                                          'high arousal','high arousal - negative', 'high arousal - positive'))

affect <- ggplot(d2, aes(names, Mean, fill = names)) + geom_bar(stat='identity') + annotate("text", x=1, y=0, label="Never") + 
  annotate("text", x=1, y=5, label="All the time") + 
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean + SE), width = .2, position=position_dodge(.9)) +
  theme_minimal() + theme(legend.position = 'none', axis.text.x  = element_text(angle=90, vjust=0.5, size = 10)) + xlab("Affect") + ylab('Average Rating') +
  expand_limits(y=c(1,5)) + geom_vline(aes(xintercept=3.5))

# ANOVA
d3 <- gather(d1, condition, rating, hap:ha)
affectaov <- aov(rating ~ condition + Error(ID), d3)
summary(affectaov)  

ttable <- pairedttable(d1[c(1,3:8)],colnames(d1)[3:8])

# Examine correlations with age
agecorr <- corrTableCI(d1[2:8])

#graphs
age_lap <- ggplot(d1, aes(Age, lap)) + geom_point() + geom_smooth(method=lm) + 
  ggtitle('Low Arousal - Positive')
age_lan <- ggplot(d1, aes(Age, lan)) + geom_point() + geom_smooth(method=lm) + 
  ggtitle('Low Arousal - Negative')
age_han <- ggplot(d1, aes(Age, han)) + geom_point() + geom_smooth(method=lm) + 
  ggtitle('High Arousal - Negative')
age_ha <- ggplot(d1, aes(Age, ha)) + geom_point() + geom_smooth(method=lm) + 
  ggtitle('High Arousal')

#multiplot(age_lap, age_lan, age_ha, age_han, cols = 2)

