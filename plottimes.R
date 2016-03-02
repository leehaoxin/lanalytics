# plot median total time it took to answer each quiz; compute median across all quizzes

# load necessary libraries
library(Hmisc)
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings
library(ggplot2)    # extra plotting capabilities
library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

load("timesToAnswerSec.Rda")

allTotalQuizTimes <- as.data.frame(timesToAnswerSec$id)

lowestQuiz=4
highestQuiz=32

for(i in lowestQuiz:highestQuiz){
  thisQuizString <- paste('Q',i,'q', sep='')
  thisQuizIndex <- grep(thisQuizString,names(timesToAnswerSec))
  itemTimes <- as.data.frame(lapply(timesToAnswerSec[,thisQuizIndex],as.numeric))
  quizTimes <- rowSums(itemTimes, na.rm = TRUE)
  quizString <- paste('Q',i)
  allTotalQuizTimes[,quizString] <- quizTimes
}

allTotalQuizTimesMin <- as.matrix(allTotalQuizTimes[,2:ncol(allTotalQuizTimes)]/60)


png('totalQuizTimesBoxPlot.png')
boxplot(allTotalQuizTimesMin,outline=FALSE,main="time to answer quizzes",xlab="Quiz",ylab="min")
dev.off()

medianQuizTime <- median(allTotalQuizTimesMin)