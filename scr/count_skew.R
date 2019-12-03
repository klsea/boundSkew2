count_skew <- function(data) {
  # calculate individual difference score - count number of skew gambles acceptances 
  subs <- unique(data$ID)
  data <- data[which(data$deg_skew != 0),]
  dt <- matrix(nrow = length(subs), ncol = 4)
  for (s in subs) {
    d0 <- data[which(data$ID == s),]
    dt[s,1] <- s
    dt[s,2] <- d0$valence[1]
    dt[s,3] <- d0$magnitude[1]
    dt[s,4] <- sum(d0$accept)
  }
  dt <- data.frame(dt)
  colnames(dt) <- c('ID', 'valence', 'magnitude', 'skew_count')
  return(dt)
}

#data = d1
#s = 1
