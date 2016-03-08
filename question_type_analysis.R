# question_type_analysis.R
# reads instructor-generated cognitive level file
# determines total number of quiz questions and questions in different categories
# depends on format of user-generated files; won't easily generalise 
# written by MI Stefan

# load uesful libraries
library(xlsx)       # import and export xlsx files
library(stringr)    # easy manipulation of strings

# set working directory
setwd("~/Work/MCM/academy_project/lanalytics")

# read input file
cogLevelData <- read.xlsx("Comparison_of_cognitive_level_2013-2014.xlsx",1)

# check dataset
head(cogLevelData)
names(cogLevelData)

# compute total questions for 2013 and 2014 (non-empty question ids)
totalQuestions2013 <- sum(!is.na(cogLevelData[,"Question.ID.2013"]))
totalQuestions2014 <- sum(!is.na(cogLevelData[,"MCM.2014.item"]))

# compute total number of quizzes (same for 2013 and 2014)

# find number of first and last quiz by parsing quiz ids from first and last rows 
# (Works only if they are non-empty, and if rows are in order)
firstString <- cogLevelData[1,"Question.ID.2013"]
firstQuizStartPos <- regexpr('z', firstString)[1] + 1
firstQuizEndPos <- regexpr('q', firstString)[1] -2
firstQuiz <- as.numeric(substr(firstString,firstQuizStartPos,firstQuizEndPos))

lastString <- cogLevelData[nrow(cogLevelData),"Question.ID.2013"]
lastQuizStartPos <- regexpr('z', lastString)[1] + 1
lastQuizEndPos <- regexpr('q', lastString)[1] -2
lastQuiz <- as.numeric(substr(lastString,lastQuizStartPos,lastQuizEndPos))

# total number of quizzes
totalNumberQuizzes <- lastQuiz-firstQuiz+1
  
# average number of items per quiz
averageQuestionsPerQuiz2013 <- totalQuestions2013/totalNumberQuizzes
averageQuestionsPerQuiz2014 <- totalQuestions2014/totalNumberQuizzes

# types of questions

# see what levels there are for 2013:
levels(cogLevelData[,"Type.1"])

# compute how many questions were MC or ManyC in 2013:
MCItems2013 <- sum(cogLevelData[,"Type.1"]=="MC",na.rm=TRUE)



