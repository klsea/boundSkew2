# individual difference measures skew
# 10.29.19 KLS updated 12.3.19

# load required packages
library(here)
library(ggplot2)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),grep('X0', colnames(dt))[1]:grep('CATCH2.4', colnames(dt))) # no symmetric trials here
d1 <- clean_skew2(d0)

#recode response to acceptance
d1$accept <- d1$response - 1

# add skew count variable
d2 <-count_skew(d1)
d2$skew_count_f <- ordered(d2$skew_count, 
                           levels= c('0','1','2','3','4','5','6','7','8','9','10'))
d2$magval <- interaction(d2$valence, d2$magnitude)

# examine distribution
ggplot(d2, aes(skew_count_f)) + geom_histogram(stat='count', binwidth=.5, position="dodge")
ggplot(d2, aes(skew_count_f, fill = magval)) + geom_histogram(stat='count', binwidth=.5, position="dodge")
