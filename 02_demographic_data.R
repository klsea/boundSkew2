# demographics
# 10.17.19 KLS updated 12.3.19

# load required packages
library(here)
library(dplyr)
#library(english)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))

# Age
demo <- c(nrow(dt), mean(dt$Age),sd(dt$Age),range(dt$Age))
write.csv(demo, here('output', 'demo.csv'), row.names = FALSE)
hist(dt$Age)

# Gender
dt$Gender <- as.character(dt$Gender)
dt$Gender <- recode(dt$Gender, "1" = "Male", "2" = "Female")
table(dt$Gender)

#Education
dt$Education <- as.character(dt$Education)
dt$Education <- recode(dt$Education, "1" = "Middle School", "2" = "High School Diploma", 
                       "3" = "Some College", "4" = "Bachelor's Degree", "5" = "Master's Degree", 
                       "6" = "Doctoral Degree" )
table(dt$Education)


#Race
dt$Race <- as.character(dt$Race)
dt$Race <- recode(dt$Race, "1" = "White/Caucasian", "2" = "Black/African American", 
                  "3" = "Asian", "4" = "Hispanic/Latino", "5" = "American Indian/Alaska Native", 
                  "6" = "Pacific Islander", "7" = "Multiracial", "8" = "Other")
table(dt$Race)

