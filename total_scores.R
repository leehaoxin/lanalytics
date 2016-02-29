## total scores.R
## using data from two successive offerings of the course
## computes scores for all quizzes, per student, per quiz, and overall
## Written by MI Stefan

## load uesful libraries
library(Hmisc)
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings
library(ggplot2)    # extra plotting capabilities
library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

# specify output directory
output_dir="total_scores/"

# specify quiz numbers
lowestQuiz=4
highestQuiz=32

# read in correctness files from 2013 and 2014 (1 for correct answer, 0 for incorrect)
correctness2013 <- read.csv('2013_output/correctness.csv')
correctness2014 <- read.csv('correctness.csv')

resultsPerStudent2013 <- data.frame()
resultsPerStudent2014 <- data.frame()

# go through every student and every quiz

# 2013
for (i in (1:nrow(correctness2013))){

  # Ignore questions that have zero in every row 
  # (questions that were not auto-graded or had no specific correct response)  
  correctness2013 <- correctness2013[, colSums(correctness2013,na.rm=TRUE)!=0]
  
  student_id <- paste(correctness2013[i,"id"],'_13',sep='')
  resultsPerStudent2013[i,'id'] <- student_id
  # go through all quizzes
  for (j in lowestQuiz:highestQuiz){
    # find all the questions belonging to this quiz
    thisQuizString <- paste('Q',j,'q', sep='')
    thisQuizIndex <- grep(thisQuizString,names(correctness2013))
    # compute sum of true answers (ignoring NA responses)
    trueSum <- sum(correctness2013[i,thisQuizIndex],na.rm=TRUE)
    quizScore <- 100*trueSum/length(correctness2013[i,thisQuizIndex])
    quizString <- paste('Q',j,'_13',sep='')
    resultsPerStudent2013[i,quizString] <- quizScore
  }
}

# 2014
for (i in (1:nrow(correctness2014))){
  
  # Ignore questions that have zero in every row 
  # (questions that were not auto-graded or had no specific correct response)
  
  correctness2014 <- correctness2014[, colSums(correctness2014,na.rm=TRUE)!=0]
  
  student_id <- paste(correctness2014[i,"id"],'_14',sep='')
  resultsPerStudent2014[i,'id'] <- student_id
  # go through all quizzes
  for (j in lowestQuiz:highestQuiz){
    # find all the questions belonging to this quiz
    thisQuizString <- paste('Q',j,'q', sep='')
    thisQuizIndex <- grep(thisQuizString,names(correctness2014))
    # compute sum of true answers (ignoring NA responses)
    trueSum <- sum(correctness2014[i,thisQuizIndex],na.rm=TRUE)
    quizScore <- 100*trueSum/length(correctness2014[i,thisQuizIndex])
    quizString <- paste('Q',j,'_14',sep='')
    resultsPerStudent2014[i,quizString] <- quizScore
  }
}

scores2013 <- as.matrix(resultsPerStudent2013[,2:ncol(resultsPerStudent2013)])
scores2014 <- as.matrix(resultsPerStudent2014[,2:ncol(resultsPerStudent2014)])
allscores <- c(scores2013,scores2014)

png('histogram_all_scores.png')
hist(allscores, breaks=10,xlab="Quiz score",main="Quiz scores 2013-2014")
dev.off()

mean_score <- mean(allscores)
mean_score_2013 <- mean(scores2013)
mean_score_2014 <- mean(scores2014)

stdev_score <- sd(allscores)
stdev_score_2013 <- sd(allscores)
stdev_score_2014 <- sd(allscores)

mean_nonzero_score <- mean(allscores[allscores>0])
mean_nonzero_score_2013 <- mean(scores2013[scores2013>0])
mean_nonzero_score_2014 <- mean(scores2014[scores2014>0])

median_score <- median(allscores)
median_score_2013 <- median(scores2013)
median_score_2014 <- median(scores2014)





