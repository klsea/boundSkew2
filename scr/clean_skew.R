clean_skew <- function(data) {
  library(reshape2)
  # make long
  d1 <- melt(data, id.vars = c('ID','Age'), variable.name = 'gamble', value.name = 'response')
  d1$v1 <- as.factor(t(as.data.frame(strsplit(as.character(d1$gamble), '_')))[,1])
  
  # valence (gain/loss/neutral)
  d1$valence[d1$v1 == 'X0'] <- 'neutral'
  d1$valence[d1$v1 == 'X0.5'] <- 'gain'
  d1$valence[d1$v1 == 'X5'] <- 'gain'
  d1$valence[d1$v1 == 'n0.5'] <- 'loss'
  d1$valence[d1$v1 == 'n5'] <- 'loss'

  # magnitude (0, 0.5, 5)
  d1$magnitude[d1$v1 == 'X0'] <- 0
  d1$magnitude[d1$v1 == 'X0.5'] <- 0.5
  d1$magnitude[d1$v1 == 'X5'] <- 5
  d1$magnitude[d1$v1 == 'n0.5'] <- 0.5
  d1$magnitude[d1$v1 == 'n5'] <- 5
  d1$v1 <- NULL

  # deg of skew (low, medium, high)
  d1$v2 <- as.factor(t(as.data.frame(strsplit(as.character(d1$gamble), '_')))[,2])

  d1$deg_skew[d1$v2 == 95 | d1$v2 == 5] <- 'Strong'
  d1$deg_skew[d1$v2 == 90 | d1$v2 == 10] <- 'Strong'
  d1$deg_skew[d1$v2 == 85 | d1$v2 == 15] <- 'Strong'
  d1$deg_skew[d1$v2 == 80 | d1$v2 == 20] <- 'Moderate'
  d1$deg_skew[d1$v2 == 75 | d1$v2 == 25] <- 'Moderate'
  d1$deg_skew[d1$v2 == 70 | d1$v2 == 30] <- 'Moderate'
  d1$deg_skew[d1$v2 == 65 | d1$v2 == 35] <- 'Weak'
  d1$deg_skew[d1$v2 == 60 | d1$v2 == 40] <- 'Weak'
  d1$deg_skew[d1$v2 == 55 | d1$v2 == 45] <- 'Weak'
  d1$deg_skew[d1$v2 == 50 | d1$v2 == 50] <- 'Symmetric'

  # dir of skew (positive or negative)
  d1$v2 <- as.numeric(as.character(d1$v2))
  d1$dir_skew[d1$v2 > 50] <- 'Negative'
  d1$dir_skew[d1$v2 < 50] <- 'Positive'
  d1$dir_skew[d1$v2 == 50] <- 'Symmetric'
  d1$v2 <- NULL
 
  # remove incomplete trials
  d2 <- d1[complete.cases(d1),]
  d3 <- d2[order(d2$ID, d2$deg_skew),]
  d4 <- d3[c(1:3,5:8,4)]
  
}

clean_skew2 <- function(data) {
  library(reshape2)
  # make long
  d1 <- melt(data, id.vars = c('ID','Age'), variable.name = 'gamble', value.name = 'response')
  d1$v1 <- as.factor(t(as.data.frame(strsplit(as.character(d1$gamble), '_')))[,1])
  
  # valence (gain/loss/neutral)
  d1$valence[d1$v1 == 'X0'] <- 'neutral'
  d1$valence[d1$v1 == 'X0.5'] <- 'gain'
  d1$valence[d1$v1 == 'X5'] <- 'gain'
  d1$valence[d1$v1 == 'n0.5'] <- 'loss'
  d1$valence[d1$v1 == 'n5'] <- 'loss'
  
  # magnitude (0, 0.5, 5)
  d1$magnitude[d1$v1 == 'X0'] <- 0
  d1$magnitude[d1$v1 == 'X0.5'] <- 0.5
  d1$magnitude[d1$v1 == 'X5'] <- 5
  d1$magnitude[d1$v1 == 'n0.5'] <- 0.5
  d1$magnitude[d1$v1 == 'n5'] <- 5
  d1$v1 <- NULL
  
  # deg of skew (low, medium, high)
  d1$v2 <- as.factor(t(as.data.frame(strsplit(as.character(d1$gamble), '_')))[,2])
  
  d1$deg_skew[d1$v2 == 95 | d1$v2 == 5] <- 9
  d1$deg_skew[d1$v2 == 90 | d1$v2 == 10] <- 8
  d1$deg_skew[d1$v2 == 85 | d1$v2 == 15] <- 7
  d1$deg_skew[d1$v2 == 80 | d1$v2 == 20] <- 6
  d1$deg_skew[d1$v2 == 75 | d1$v2 == 25] <- 5
  d1$deg_skew[d1$v2 == 70 | d1$v2 == 30] <- 4
  d1$deg_skew[d1$v2 == 65 | d1$v2 == 35] <- 3
  d1$deg_skew[d1$v2 == 60 | d1$v2 == 40] <- 2
  d1$deg_skew[d1$v2 == 55 | d1$v2 == 45] <- 1
  d1$deg_skew[d1$v2 == 50 | d1$v2 == 50] <- 0
  
  # dir of skew (positive or negative)
  d1$v2 <- as.numeric(as.character(d1$v2))
  d1$dir_skew[d1$v2 > 50] <- 'Negative'
  d1$dir_skew[d1$v2 < 50] <- 'Positive'
  d1$dir_skew[d1$v2 == 50] <- 'Symmetric'
  d1$v2 <- NULL
  
  # remove incomplete trials
  d2 <- d1[complete.cases(d1),]
  d3 <- d2[order(d2$ID, d2$deg_skew),]
  d4 <- d3[c(1:3,5:8,4)]
  
}
