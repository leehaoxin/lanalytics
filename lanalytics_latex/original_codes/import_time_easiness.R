## import_time_easiness.R
## short helper script to import time and easiness information from xlsx
## (for use with data created earlier in the project with MATLAB)
## if you are analysing your data from scratch, you will never need this!
## Written by MI Stefan

## load necessary libaries
library(stringr)    # manipulate strings

## location of files to read in 
fileDir <- "/home/melanie/Work/MCM/academy_project/matlab_tables/"

## names of files to read in
timesFileName <- "2013_median_times_minutes.csv"
easinessFileName <- "2013_difficulty.csv"
questionFileName <- "2013_MC_questions.csv"

timesFile <- paste(fileDir,timesFileName,sep="")
easinessFile <- paste(fileDir,easinessFileName,sep="")
questionFile <- paste(fileDir,questionFileName,sep="")

## read in csv files
medianTimes <- read.csv(timesFile, header=FALSE)
easiness <- read.csv(easinessFile,header = FALSE)
questions <- read.csv(questionFile)

## parse question names
questionNames <- list()
unparsedQuestionNames <- names(questions)

for (i in 1:length(unparsedQuestionNames)){
    name = unparsedQuestionNames[i]
    z <- str_locate(pattern="z",name)    
    q <- str_locate(pattern="q",name)    
    quiz <- substr(name,z+1,q-2)
    question <- substr(name,q+1,str_length(name))
    questionString <- paste("Q",quiz,"q",question,sep="")
    questionNames <- cbind(questionNames,questionString)    
}


## create data structures
easiness <- as.numeric(t(easiness))
names(easiness) <-  questionNames

medianTimes <- as.numeric(t(medianTimes))
names(medianTimes) <-  questionNames


## save as (appropriately named) data frames
save(easiness, file="easiness_2013.Rda")
save(medianTimes, file="median_Times_2013.Rda")



