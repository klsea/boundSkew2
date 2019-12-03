# graph strategy data
# 10.29.19 KLS updated 12.3.19

# load required packages
library(here)
library(ggplot2)
library(matrixStats)
library(tidyr)

# load source functions
source(here('scr', 'isolate_skew.R'))
source(here('scr', 'multiplot.R'))
source(here('scr', 'colorize_variable.R'))
source(here('scr', 'SummarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here("data", "bound_skew2_data.csv"))

# separate strategy
d0 <- isolate_skew(dt,c(1,2), grep('Q241', colnames(dt)):grep('Q251', colnames(dt)))
colnames(d0) <- c('ID', 'Age', 'gut', 'math', 'win.money', 'lose.money', 'win.likely', 'lose.likely')

# Graph means
d1 <- cbind(colMeans(d0), apply(d0, 2, sd), apply(d0, 2, sd)/sqrt(nrow(d0)))
d1 <- d1[3:nrow(d1),]
colnames(d1) <- c('Mean', 'SD', 'SE')
names <- rownames(d1); rownames(d1) <- NULL
d1 <- cbind(names, d1)
d1 <- data.frame(d1)
d1$Mean <- as.numeric(as.character(d1$Mean))
d1$SD <- as.numeric(as.character(d1$SD))
d1$SE <- as.numeric(as.character(d1$SE))
d1$names <- factor(d1$names, levels = c('gut', 'math', 'win.money', 'lose.money', 
                                        'win.likely', 'lose.likely'))

ggplot(d1, aes(names, Mean, fill = names)) + geom_bar(stat='identity') + 
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean + SE), width = .2, position=position_dodge(.9)) +
  theme_minimal() + theme(legend.position = 'none') + xlab("Strategy") + ylab('Average Rating') +
  expand_limits(y=c(1,5)) + geom_vline(aes(xintercept=2.5))
ggsave('strategy2.pdf', path = 'figs/')

# statistical tests
# compare gut vs mathematical strategy
gutvmath <- t.test(d0$gut, d0$math, paired = TRUE)
saveRDS(gutvmath, here('output', 'gutvmath2.RDS'))

# compare aspects of gamble
d2 <- gather(d0, question, rating, win.money:lose.likely, factor_key = TRUE)[,c(1:2, 5:6)]
d3 <- cbind(d2, t(as.data.frame(strsplit(as.character(d2$question), '[.]'))))
colnames(d3) <- c( "ID", "Age", "question","rating","valence", "parameter")
rownames(d3) <- NULL

valvparam <- aov(rating ~ valence * parameter + Error(ID), data = d3)
summary(valvparam)
saveRDS(valvparam, here('output', 'valvparam2.RDS'))

# Graph distributions
gut_color <- colorize_variable(d0$gut)
p1 <- ggplot(d0, aes(Age, gut))
p2 <- ggplot(d0, aes(gut, fill = gut_color))
fancy_graph(p1,p2, gut_color, 'Gut Feeling')

math_color <- colorize_variable(d0$math)
p1 <- ggplot(d0, aes(Age, math))
p2 <- ggplot(d0, aes(math, fill = math_color))
fancy_graph(p1,p2, math_color, 'Mathematically')

win.money_color <- colorize_variable(d0$win.money)
p1 <- ggplot(d0, aes(Age, win.money))
p2 <- ggplot(d0, aes(win.money, fill = win.money_color))
fancy_graph(p1,p2, win.money_color, 'Win Money')

lose.money_color <- colorize_variable(d0$lose.money)
p1 <- ggplot(d0, aes(Age, lose.money))
p2 <- ggplot(d0, aes(lose.money, fill = lose.money_color))
fancy_graph(p1,p2, lose.money_color, 'Lose Money')

win.likely_color <- colorize_variable(d0$win.likely)
p1 <- ggplot(d0, aes(Age, win.likely))
p2 <- ggplot(d0, aes(win.likely, fill = win.likely_color))
fancy_graph(p1,p2, win.likely_color, 'Win Likely')

lose.likely_color <- colorize_variable(d0$lose.likely)
p1 <- ggplot(d0, aes(Age, lose.likely))
p2 <- ggplot(d0, aes(lose.likely, fill = lose.likely_color))
fancy_graph(p1,p2, lose.likely_color, 'Lose Likely')

rm(p1,p2)

d0$diffstrategy <- d0$math - d0$gut
ggplot(d0, aes(Age, diffstrategy)) + geom_point() + geom_smooth(method = 'lm', fill = 'blue') +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(color = 'red', fill = 'red')) 



