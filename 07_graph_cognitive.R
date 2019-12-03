# visualize numeracy and graph literacy
# 11.25.19 KLS

# load required packages
library(here)
library(ggplot2)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'isolate_measure.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))
dict <- read.csv(here("data", "bound_skew2_data_dictionary.csv"))

# isolate graph literacy
Sys.setlocale('LC_ALL','C')
qs <- as.character(dict$Question)
first <- grep("bar charts", qs)
last <- grep("newspapers", qs)
d0 <- isolate_skew(dt,c(1,2), first:last)

# graph literacy score
d1 <- score_graph_lit(d0)

# visualize graph literacy
sgl1 <- ggplot(d1, (aes(x=graph_lit))) + geom_histogram(binwidth = 0.5) +
  geom_vline(aes(xintercept=mean(graph_lit, na.rm=T)), color="red", linetype="dashed", size=1)

sgl2 <- ggplot(d1, aes(Age, graph_lit)) + geom_point() + geom_smooth(method=lm)

# isolate numeracy
first <- grep("six-sided", qs)
last <- grep("10,000 doctors", qs)
d2 <- isolate_skew(dt,c(1,2), first:last)

# score numeracy
ans_key <- create_num_answer_key()
d3 <- as.data.frame(score_num(d2, ans_key))

# graph numeracy
num1 <- ggplot(d3, (aes(x=Numeracy))) + geom_histogram(binwidth = 0.5) +
  geom_vline(aes(xintercept=mean(Numeracy, na.rm=T)), color="red", linetype="dashed", size=1)

num2 <- ggplot(d3, aes(Age, Numeracy)) + geom_point() + geom_smooth(method=lm)
