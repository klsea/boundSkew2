# create color factor

colorize_variable <- function(variable) {
  new_variable <- factor(variable)
  levels(new_variable)[1] <- 'Strongly disagree'
  levels(new_variable)[2] <- 'Somewhat disagree'
  levels(new_variable)[3] <- 'Neutral'
  levels(new_variable)[4] <- 'Somewhat agree'
  levels(new_variable)[5] <- 'Strongly agree'
  return(new_variable)
}

fancy_graph <- function(plot1, plot2, colorvar, ylabel) {
  g1 <- plot1 + geom_point(aes(color = colorvar)) + 
    geom_smooth(method=lm) + theme_minimal() + theme(legend.position = 'none') + 
    expand_limits(y=c(0.5,5.5)) + ylab(ylabel)
  g2 <- plot2 + geom_histogram(stat='count') + coord_flip() + 
    theme_minimal() + theme(legend.position = 'none', axis.title.y = element_blank(), 
                            axis.text.y = element_blank()) + xlab('Count')
  multiplot(g1,g2, cols = 2)
}


