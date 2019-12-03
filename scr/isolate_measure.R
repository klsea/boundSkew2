isolate_measure <- function(data, term, datadict) {
  # input: data = data table
  # term = character 
  # datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  data[,c(9,grep(term, datadict[,2]))]
}

add_correct_avi_labels <- function(avi, term1, term2, datadict) {
  # input: avi 
  # term = character
  # datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  qs <- as.character(dict$Question)
  d0 <- as.character(datadict[grep(term1, qs):grep(term2, qs),2])
  d1 <- d0[1:6]
  d1 <- gsub('Listed below are a number of words that describe feelings. Some of the feelings are very similar to\neach other, whereas others are very different from each other. Read each word and then select how\noften YOU ACTUALLY HAVE that feeling over the course of a typical week:\nOver the course of a typical week, I ACTUALLY feel\211\333_ - ', '', d1)
  d2 <- d0[7:24]
  d2 <- gsub("Over the course of a typical week, I ACTUALLY feel\211\333_ - ", "", d2)
  d3 <- c(d1,d2)
  colnames(avi) <- c('ID', 'Age', d3)
  return(avi)
}

score_avi <- function(avi) {
  n_items <- length(avi)
  avi$hap <- (avi$Enthusiastic + avi$Excited + avi$Strong + avi$Elated) / 4 
  avi$lap <- (avi$Calm + avi$Relaxed + avi$Peaceful) / 3 # modified missing Serene
  #avi$lap <- (avi$Calm + avi$Relaxed + avi$Peaceful + avi$Serene) / 4
  avi$la <- (avi$Quiet + avi$Passive + avi$Still) / 3
  #avi$pos <- (avi$Content + avi$Happy + avi$Satisfied ) /3 # Missing all
  avi$lan <- (avi$Dull + avi$Sleepy + avi$Sluggish) / 3
  avi$han <- (avi$Fearful + avi$Hostile + avi$Nervous) /3
  avi$ha <- (avi$Aroused + avi$Surprised + avi$Astonished) / 3
  #avi$neg <- (avi$Lonely + avi$Sad + avi$Unhappy ) /3 # Missing all but lonely
  end <- length(avi)
  return(avi[,c(1:2,(n_items+1):end)])
}

isolate_measure2 <- function(data, first_term, last_term, datadict) {
  #input:
  #data = data table 
  #first_term = first term in questionnaire section (character)
  #last_term = last term in questionnaire section (character)
  #datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  datadict$Question <- as.character(datadict$Question)
  first <- as.data.frame(grep(first_term, datadict$Question))
  last <- as.data.frame(grep(last_term, datadict$Question))
  f1 <- first[1,]
  l1 <- last[length(last),]
  data[,c(9, f1:l1)]
}

add_graph_lit_labels <- function(measure, first_term, last_term, datadict ) {
  #isolate_measure2(graph_lit)# would be great to use this line if function can be auto input rather than typing out again
  fst <- as.data.frame(grep(first_term, datadict[,2]))
  lst <- as.data.frame(grep(last_term, datadict[,2]))
  f1 <- fst[1,]
  l1 <- lst[length(lst),]
  ratings <- as.character(datadict[f1:l1,2])
  colnames(measure) <- c('SubID', 'Age', ratings)
  return(measure)
}
  
score_graph_lit <- function(data) {
  # input: data = data table of graph literacy only
  dt <- cbind.data.frame(data$ID, data$Age, rowSums(data[3:length(d0)]))
  colnames(dt) <- c('ID', 'Age', 'graph_lit')
  return(dt)
}

hat_which_measure <- function(answers, response) {
  rspn <- match(response, answers)
  if(is.na(rspn)){
    print('incorrect')
  } else {
    print('correct')
  }
}

create_num_answer_key <- function() {
  QNAMES <- c("Q1","Q2","Q3", 'Q4', 'Q5','Q6','Q7','Q8a','Q8b', 'Q9', 'Q10','Q11','Q12','Q13','Q14')
  Q1 <- c("half the time", "50%", 490:510, 1:2)
  Q2 <- c("10", "10 people", "0.01", ".01", "1.00e+01")
  Q3 <- c("0.1%", "0.1", '0.1000')
  Q4 <- c(3)
  Q5 <- c(2)
  Q6 <- c(2, "2%", '2 out of 100', '2% in ten years', '2 percent', '1% in 5 years')
  Q7 <- c(2, "2%", '2 out of 100', '2 in 100 years', '1 in 50 in ten years', '2 in a hundred', '2 in 100','2 of 100', '1 in 50', '2 in 100 in ten years')
  Q8a <- c(10)
  Q8b <- c(100)
  Q9 <- c(20, '20%', '1 in 5', '1 in5')
  Q10 <- c('5 people', '5.0e+00', 5)
  Q11 <- c(1)
  Q12 <- c('9/18', '1/2', '50%', '50 percent')
  Q13 <- c(64, 64.00)
  Q14 <- c(3)
  
  QNUMBS <- list(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8a,Q8b,Q9,Q10,Q11,Q12,Q13,Q14);QNUMBS
  return(QNUMBS)
  #num_ans_key <- data.frame(QNAMES, I(QNUMBS));num_ans_key
  #num_ans_key[1,2]
  #return(num_ans_key)
}

score_num <- function(num, num_ans_key) {
  dt <- matrix(, nrow = nrow(num), ncol = ncol(num))
  for (col in 3:ncol(num)){
    for (row in 1:nrow(num)){
      dt[row, col] <- ifelse(as.character(num[row,col]) %in% num_ans_key[[col-2]], 1, 0)
    }
  }
  score <- matrix(, nrow = nrow(num), ncol = 3)
  score[,1] <- num[,1]
  score[,2] <- num[,2]
  score[,3] <- rowSums(dt[,3:17])
  colnames(score) <- c('ID', 'Age', 'Numeracy')
  return(score)
}

