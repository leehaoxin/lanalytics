# plot median total time it took to answer each quiz; compute median across all quizzes
# to do: compute this for both 2013 and 2014

# load necessary libraries
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings
library(ggplot2)    # extra plotting capabilities
library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

# # load 2013 data
load(file="2013_output/timesToAnswerSec.Rda")
timesToAnswerSec2013 <- timesToAnswerSec
allTotalQuizTimes2013 <- as.data.frame(timesToAnswerSec2013$id)


# load 2014 data
load("timesToAnswerSec.Rda")
timesToAnswerSec2014 <- timesToAnswerSec
allTotalQuizTimes2014 <- as.data.frame(timesToAnswerSec2014$id)


lowestQuiz=4
highestQuiz=32

# get data frames of times per quiz
for(i in lowestQuiz:highestQuiz){
  thisQuizString <- paste('Q',i,'q', sep='')
  quizString <- paste('Q',i,sep='')
    
  thisQuizIndex2013 <- grep(thisQuizString,names(timesToAnswerSec2013))
  itemTimes2013 <- as.data.frame(lapply(timesToAnswerSec2013[,thisQuizIndex2013],as.numeric))
  quizTimes2013 <- rowSums(itemTimes2013, na.rm = TRUE)
    
  thisQuizIndex2014 <- grep(thisQuizString,names(timesToAnswerSec2014))
  itemTimes2014 <- as.data.frame(lapply(timesToAnswerSec2014[,thisQuizIndex2014],as.numeric))
  quizTimes2014 <- rowSums(itemTimes2014, na.rm = TRUE)
  
  allTotalQuizTimes2013[,quizString] <- quizTimes2013
  allTotalQuizTimes2014[,quizString] <- quizTimes2014  
}

allTotalQuizTimesMin2013 <- as.matrix(allTotalQuizTimes2013[,2:ncol(allTotalQuizTimes2013)]/60)
allTotalQuizTimesMin2014 <- as.matrix(allTotalQuizTimes2014[,2:ncol(allTotalQuizTimes2014)]/60)


png('totalQuizTimesBoxPlot2013.png')
boxplot(allTotalQuizTimesMin2013,outline=FALSE,main="time to answer quizzes (2013)",xlab="Quiz",ylab="min")
dev.off()

png('totalQuizTimesBoxPlot2014.png')
boxplot(allTotalQuizTimesMin2014,outline=FALSE,main="time to answer quizzes (2014)",xlab="Quiz",ylab="min")
dev.off()


# medianQuizTime <- median(allTotalQuizTimesMin)

