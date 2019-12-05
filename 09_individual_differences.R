# Individual differences analysis
# Create 11.25.19 KLS from prior scripts 
# updated 12.3.19 for Skew 2

# load required packages
library(here)
library(Hmisc)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'isolate_measure.R'))
source(here('scr', 'clean_skew.R'))
source(here('scr', 'count_skew.R'))
source(here('scr', 'corrTableCI.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))
dict <- read.csv(here("data", "bound_skew2_data_dictionary.csv"))

# separate skew
d0 <- isolate_skew(dt,c(1,2),grep('X0', colnames(dt))[1]:grep('CATCH2.4', colnames(dt))) # no symmetric trials here
d1 <- clean_skew2(d0)

#recode response to acceptance
d1$accept <- d1$response - 1

# add skew count variable
d2 <-count_skew(d1)
rm(d0, d1)

# separate strategy
d0 <- isolate_skew(dt,c(1,2), grep('Q241', colnames(dt)):grep('Q251', colnames(dt)))
colnames(d0) <- c('ID', 'Age', 'gut', 'math', 'win.money', 'lose.money', 'win.likely', 'lose.likely')

# merge skew and strategy data frames
d3 <- merge(d2,d0, by = 'ID')
d3 <- d3[c(1,5,2:4,6:11)]
d3$skew_count <- as.integer(as.character(d3$skew_count))
rm(d0,d2)

# isolate AVI data
Sys.setlocale('LC_ALL','C')
qs <- as.character(dict$Question)
first <- grep("Enthusiastic", qs); last <- grep("Lonely", qs) # use if full measure: last <- grep("Serene", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# add labels to AVI data
d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Lonely", dict) # use if full measure: d0 <- add_correct_avi_labels(d0, "Enthusiastic", "Serene", dict)
d1 <- score_avi(d0)

# merge affect to skew+strategy data frame
d4 <- merge(d3, d1, by = c('ID', 'Age'))
rm(d1,d3)

# isolate graph literacy
first <- grep("bar charts", qs); last <- grep("newspapers", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# graph literacy score
d1 <- score_graph_lit(d0)
rm(d0)

# isolate numeracy
first <- grep("six-sided", qs); last <- grep("10,000 doctors", qs)
d2 <- isolate_skew(dt,c(1,2), first:last)

# score numeracy
ans_key <- create_num_answer_key()
d3 <- as.data.frame(score_num(d2, ans_key))
rm(ans_key, d2)

# merge cognitive to skew+strategy+affect data frame
d5 <- merge(d4, d1, by = c('ID', 'Age')); d6 <- merge(d5, d3, by = c('ID', 'Age'))
rm(d4, d1,d5,d3)

# identify begining and end of 
d1 <- isolate_skew(dt, 1, grep('Q173', colnames(dt)):grep('Q27', colnames(dt)))

# investment & fraud
d2 <- data.frame(d1$ID, (d1$Q173 - 2)*-1, d1$Q177, d1[11:14])
colnames(d2) <- c('ID', 'lost_invest', 'why_lost', 'detect_fraud', 'likely_fraud', 'high_pressure', 'avoid_fraud')

# merge real world with skew+strategy+affect+cog
d7 <- merge(d6, d2, by = 'ID')
rm(d1,d2,d6)

# correlations
end <- ncol(d7)-1
s2_corr <- rcorr(as.matrix(d7[c(5:20, 22:end)]))
saveRDS(s2_corr, here('output', 's2_corr.RDS'))
s2_corrCI <- corrTableCI(d7[c(5:20, 22:end)])
saveRDS(s2_corrCI, here('output', 's2_corrCI.RDS'))

# models - strategy
d7$magval <- interaction(d7$magnitude, d7$valence)
m1 <- lm(skew_count ~ gut + magval, data = d7)
summary(m1)
m2 <- lm(skew_count ~ win.money + magval, data = d7)
summary(m2)
m3 <- lm(skew_count ~ lose.money + magval, data = d7)
summary(m3)
m4 <- lm(skew_count ~ lose.likely + magval, data = d7)
summary(m4)
m5 <- lm(skew_count ~ math + magval, data = d7)
summary(m5)
m6 <- lm(skew_count ~ win.likely + magval, data = d7)
summary(m6)

# models - affect
m7 <- lm(skew_count ~ hap + magval, data = d7)
summary(m7)
m8 <- lm(skew_count ~ lap + magval, data = d7)
summary(m8)
m9 <- lm(skew_count ~ la + magval, data = d7)
summary(m9)
m10 <- lm(skew_count ~ lan + magval, data = d7)
summary(m10)
m11 <- lm(skew_count ~ han + magval, data = d7)
summary(m11)
m12 <- lm(skew_count ~ ha + magval, data = d7)
summary(m12)

# models - cognitive
d7$skew_count <- as.integer(as.character(d7$skew_count))
m13 <- lm(skew_count ~ graph_lit + magval, data = d7)
summary(m13)
m14 <- lm(skew_count ~ Numeracy + magval, data = d7)
summary(m14)

# models - real world
d7$skew_count <- as.numeric(as.character(d7$skew_count))
m15 <- lm(skew_count ~ lost_invest + magval, data = d7)
summary(m15)
m16 <- lm(skew_count ~ detect_fraud + magval, data = d7)
summary(m16)
m17 <- lm(skew_count ~ likely_fraud + magval, data = d7)
summary(m17)
m18 <- lm(skew_count ~ high_pressure + magval, data = d7)
summary(m18)

