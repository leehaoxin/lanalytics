# question_type_analysis.R
# reads instructor-generated cognitive level file
# determines total number of quiz questions and questions in different categories
# depends on format of user-generated files; won't easily generalise 
# written by MI Stefan

# load uesful libraries
library(xlsx)       # import and export xlsx files

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

# compute total number of quizzes for 2013 and 2014 (using quiz number in question ids)


