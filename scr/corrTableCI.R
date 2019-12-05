corrTableCI <- function(data) {
  ## Input columns of data you would like to correlate 
  ## Creates a table with 95% confidence intervals
  #source('~/Dropbox (Personal)/Functions/CIcorr.R')
  library(Hmisc)
  #correlations <- rcorr(as.matrix(na.omit(data)));
  correlations <- rcorr(as.matrix(data));
  table <- matrix(0, nrow=length(data),ncol=length(data))
  for (x in 1:length(data)) {
    for (y in 1:length(data)) {
      ci <- CIcorr(.05,correlations$r[x,y],1,nrow(data))
      table[x,y] <- paste0(as.character(round(correlations$r[x,y],2)), ' [', as.character(round(ci[1],2)), ', ', as.character(round(ci[2],2)), ']')
    }
  }
  corrTable <- as.data.frame(table); colnames(corrTable) <- colnames(data); corrTable <- cbind(colnames(data), corrTable)
}      

CIcorr <- function(alpha, corr, s, n) {
  # Computes a confidence interval for a (partial) Pearson correlation
  # Arguments: 
  #   alpha: alpha value for 1-alpha confidence
  #   corr:  sample value of (partial) correlation 
  #   s:     number of control variables
  #   n:     sample size
  # Returns:
  #   confidence interval
  z <- qnorm(1 - alpha/2)
  se <- sqrt(1/((n - s - 3)))
  zr <- log((1 + corr)/(1 - corr))/2
  LL0 <- zr - z*se
  UL0 <- zr + z*se
  LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1)
  UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1)
  CI <- c(LL, UL)
  return(CI)
}