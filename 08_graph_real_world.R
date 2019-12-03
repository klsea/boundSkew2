# investment and fraud
# 11.5.19 KLS updated 12.3.19

# load required packages
library(here)
library(ggplot2)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))
source(here('scr', 'colorize_variable.R'))
source(here('scr', 'multiplot.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))

# identify begining and end of 
d1 <- isolate_skew(dt, c(1,2), grep('Q173', colnames(dt)):grep('Q27', colnames(dt)))

# investment & fraud
d2 <- data.frame(d1$ID, d1$Age, (d1$Q173 - 2)*-1, d1$Q177, d1$Q11, d1$Q13, d1$Q15, d1$Q17)
colnames(d2) <- c('ID', 'Age', 'lost_invest', 'why_lost', 'detect_fraud', 'likely_fraud', 'high_pressure', 'avoid_fraud')
d3 <- d2

# counts
n_lost_invest <- nrow(d2[which(d2$lost_invest == 1),])
n_total <- nrow(d2)
n_fraud <- nrow(d2[which(d2$why_lost == 4),])
real_world_n <- c(n_total, n_fraud, n_lost_invest)
write.csv(real_world_n, here('output', 'real_world2.csv'), row.names = FALSE)

library(plyr)

# investment graph
# Have you ever made an investment where you lost some or all of the money you invested? 
d2$lost_invest <- factor(d2$lost_invest)
d2$lost_invest <- revalue(d2$lost_invest, c("0"="No", "1"="Yes"))
invest1 <- ggplot(d2, aes(lost_invest)) + geom_histogram(binwidth = .5, stat = "count") + 
  ggtitle('Have you ever made an investment where you lost some \nor all of the money you invested?')

# If yes, Which statement below best describes why you lost money? 
d2$why_lost <- factor(d2$why_lost)
d2$why_lost <- revalue(d2$why_lost, c('1'='Bad Investment', '2'='Market Downturn', 
                                      '3'='Lack of Knowledge', '4'='Misled or Defrauded', '5'='Other'))
invest2 <- ggplot(d2[which(d2$lost_invest=='Yes'),], aes(why_lost, fill = why_lost)) + 
  geom_histogram(binwidth = .5, stat = "count") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) + guides(fill=FALSE) + 
  ggtitle('If yes, which statement below best describes why you lost money?') + 
  theme(axis.title.x = element_blank())

# fraud
## How able are you to detect a fraudulent investment? (select one)
d2$detect_fraud <- factor(d2$detect_fraud)
d2$detect_fraud <- revalue(d2$detect_fraud , c('1' = 'not able to detect', '7' = 'very able to detect'))
fraud1 <- ggplot(d2, aes(detect_fraud)) + geom_histogram(binwidth = .5, stat = "count") +
  ggtitle("How able are you to detect a fraudulent investment?") + theme(axis.title.x = element_blank())

# How likely are you to make a fraudulent investment? (select one)
d2$likely_fraud <- factor(d2$likely_fraud)
d2$likely_fraud <- revalue(d2$likely_fraud , c('1' = 'not at all likely', '7' = 'very likely'))
fraud2 <- ggplot(d2, aes(likely_fraud)) + geom_histogram(binwidth = .5, stat = "count") + 
  ggtitle("How likely are you to make a fraudulent investment?") + theme(axis.title.x = element_blank())
# 1 = not at all likely, 7 = very likely

# How able are you to resist high-pressure sales tactics when buying investments? (select one)
d2$high_pressure <- factor(d2$high_pressure)
d2$high_pressure <- revalue(d2$high_pressure , c('1' = 'not at all able to resist', '7' = 'very able to resist'))
fraud3 <- ggplot(d2, aes(high_pressure)) + geom_histogram(binwidth = .5, stat = "count") + 
  ggtitle("How able are you to resist high-pressure sales tactics \nwhen buying investments?") +
  theme(axis.title.x = element_blank())
# 1 = not at all able to resist, 7 = very able to resist

# Have you ever encountered a situation in which someone was pitching a potentially fraudulent investment, but you avoided investing or losing money?  (select one)
#gplot(d2, aes(avoid_fraud)) + geom_histogram(binwidth = .5, stat = "count") - bad question


