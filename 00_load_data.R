# Clean skew data
# 7.31.19 KLS

# load required packages
library(here)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv('~/Dropbox (MCAB Lab)/MCAB/data/skew2/data/skew_full_raw.csv')
dt <- dt[-c(10:17)]
dd <- t(dt[1,])
dt <- dt[-c(1:2),]
dt$Age <- as.integer(as.character(dt$Age))
dt$Age <- dt$Age + 19

write.csv(dd, here("data", "bound_skew1_data_dictionary.csv"))
write.csv(dt, here("data", "bound_skew1_data.csv"), row.names = FALSE)
