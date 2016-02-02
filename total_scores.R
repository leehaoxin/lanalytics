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

resultsPerStudent <- data.frame()

# go through every student and every quiz

# 2013
for (i in (1:nrow(correctness2013))){
  student_id <- paste(correctness2013[i,"id"],'_13',sep='')
  resultsPerStudent[i,'id'] <- student_id
  # go through all quizzes
  for (j in lowestQuiz:highestQuiz){
    # find all the questions belonging to this quiz
    thisQuizString <- paste('Q',j,'q', sep='')
    thisQuizIndex <- grep(thisQuizString,names(correctness2013))
    # compute sum of true answers (ignoring NA responses)
    trueSum <- sum(correctness2013[i,thisQuizIndex],na.rm=TRUE)
    quizScore <- 100*trueSum/length(correctness2013[i,thisQuizIndex])
    quizString <- paste('Q',j,'_13',sep='')
    resultsPerStudent[i,quizString] <- quizScore
  }
}













