## easiness_time_level.R
## takes easiness data created with easiness_time.R
## takes time to answer each question created with order_time.R
## takes instructor-rated level of each question (1=low, 3=high)
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## Written by MI Stefan

## load necessary libraries
library(Hmisc)
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings

# read in easiness and median time data 
load(file="easinessPlot.Rda")
load(file="medianTimesPlot.Rda")

# read in xls with cognitive level rating
cognitiveLevel <- read.xlsx("./Quiz2013-14_cognitive level_HB.xlsx",1)

# create empty data frame 
# holds easiness, median Time, cognitive level, quiz number, question number
allQuestions <- data.frame(matrix(nrow=0,ncol=5))

# provide question column and rating column
questionCol = 3
ratingCol = 5

# provide questions to ignore (can be empty), as determined by instructor
# here, we are ignoring question 12 of Quiz 9 (duplicate question)
ignore = "Q9_q12"


# go through all question for which we have a time and easiness rating
for (i in 1:length(names(medianTimesPlot))){
    # make empty dataframe to hold all info for this question
    thisQuestion <- data.frame(matrix(nrow=1,ncol=5))
    colnames(thisQuestion) <- c("easiness","time", "level", "quiz","question")
    
    # get easiness and time
    thisQuestion$easiness <- rateCorrectPlot[i]
    thisQuestion$time <- medianTimesPlot[i]
    
    # get quiz and question number 
    quizQuestion <- names(medianTimesPlot)[i]
    q <- str_locate(pattern="q",quizQuestion)
    quiz <- substr(quizQuestion,2,q-1)
    thisQuestion$quiz <- quiz
    question <- substr(quizQuestion,q+1,str_length(quizQuestion))
    thisQuestion$question <- question
     
    # find this quiz and question number (with underscore)
    questionString = paste("Q",quiz,"_q",question,sep="")
    if (ignore != questionString){        
        cogIndex <- which(cognitiveLevel[,questionCol]==questionString)
        if (length(cogIndex >0)){
            level <- cognitiveLevel[cogIndex,ratingCol]
            thisQuestion$level <- level
            rbind(allQuestions,thisQuestion)
             
        }
    }
}
    
    





