## easiness_time_level_two_years.R
## using data from two successive offerings of the course
## takes time to answer each question created with order_time.R
## takes instructor-rated level of each question (1=low, 3=high)
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## shows data from both years side-by-side
## Written by MI Stefan

## load necessary libraries
library(Hmisc)
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings
library(ggplot2)    # extra plotting capabilities
library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

# specify output directory
output_dir="2013_2014_compare/"

# specify both years
year1 = "2013"
year2 = "2014"

# read in easiness and median time data for both years
# in our case, I have 2014 data in easinessPlot.Rda and medianTimesPlot.Rda
load(file="2013_output/easinessPlot.Rda")
load(file="2013_output/medianTimesPlot.Rda")
year1easiness <- rateCorrectPlot
year1time <- medianTimesPlot

load(file="easinessPlot.Rda")
load(file="medianTimesPlot.Rda")
year2easiness <- rateCorrectPlot
year2time <- medianTimesPlot

# read in xls with challenge level rating
challengeLevel <- read.xlsx("Comparison_of_cognitive_level_2013-2014.xlsx",1)
before = nrow(challengeLevel)

# remove rows that are all nan
challengeLevel <- challengeLevel[rowSums(is.na(challengeLevel))!=ncol(challengeLevel), ]

# check data set
head(challengeLevel)
names(challengeLevel)
                            

# create empty data frame 
# holds easiness, median Time, challenge level, quiz number, question number
allQuestions2013 <- data.frame(matrix(nrow=0,ncol=5))
allQuestions2014 <- data.frame(matrix(nrow=0,ncol=5))
allQuestionsBothYears <- data.frame(matrix(nrow=0,ncol=10))

# provide question column and rating column both for 2013 and 2014

itemIDCol2013 <- "Question.ID.2013"
itemIDCol2014 <- "MCM.2014.item"
ratingCol2013 <- "Rating.HB"
ratingCol2014 <- "Rating.HB."
typeCol2013 <- "Type"
typeCol2014 <- "Type.1"

######### start analysis

# find number of first and last quiz by parsing quiz ids from first and last rows 
# (Works only if they are non-empty, and if rows are in order)
firstString <- challengeLevel[1,"Question.ID.2013"]
firstQuizStartPos <- regexpr('z', firstString)[1] + 1
firstQuizEndPos <- regexpr('q', firstString)[1] -2
firstQuiz <- as.numeric(substr(firstString,firstQuizStartPos,firstQuizEndPos))

lastString <- challengeLevel[nrow(challengeLevel),"Question.ID.2013"]
lastQuizStartPos <- regexpr('z', lastString)[1] + 1
lastQuizEndPos <- regexpr('q', lastString)[1] -2
lastQuiz <- as.numeric(substr(lastString,lastQuizStartPos,lastQuizEndPos))


## go through all lines of challengeLevel
for (row in 1:nrow(challengeLevel)){
  exists2013 <- 0
  exists2014 <- 0
  
  # check that entry for 2013 exists and is Multiple Choice.
  # if so, record data for 2013
  if (!is.na(challengeLevel[row,typeCol2013]) & challengeLevel[row,typeCol2013]=="MC"){
    exists2013 <- 1
    # make empty dataframe to hold all info for this question
    thisQuestion2013 <- data.frame(matrix(nrow=1,ncol=5))
    colnames(thisQuestion2013) <- c("quiz_13","question_13","easiness_13","time_13", "level_13")
    
    # get quiz and question number 
    quizQuestion <- challengeLevel[row,itemIDCol2013]
    q <- str_locate(pattern="q",quizQuestion)
    quizString <- str_locate(pattern="Quiz",quizQuestion)
    quiz <- substr(quizQuestion,quizString+4,q-2)
    question <- substr(quizQuestion,q+1,str_length(quizQuestion))
    
    # add quiz and question info to data frame
    thisQuestion2013$quiz_13 <- quiz
    thisQuestion2013$question_13 <- question
    
    # get easiness and time
    questionCol <- paste("Q",quiz,"q",question,sep="")
    thisQuestion2013$easiness_13 <- year1easiness[questionCol]
    thisQuestion2013$time_13 <- year1time[questionCol]
    
    # get challenge level
    thisQuestion2013$level_13 <- challengeLevel[row,ratingCol2013]
    
    # add to 2013 dataframe
    allQuestions2013 <- rbind(allQuestions2013,thisQuestion2013)
    }
  
  
  # check that entry for 2014 exists and is Multiple Choice.
  # if so, record data for 2014  
  if (!is.na(challengeLevel[row,typeCol2014]) & challengeLevel[row,typeCol2014]=="MC"){
    exists2014 <- 1
    # make empty dataframe to hold all info for this question
    thisQuestion2014 <- data.frame(matrix(nrow=1,ncol=5))
    colnames(thisQuestion2014) <- c("quiz_14","question_14","easiness_14","time_14", "level_14")
    
    # get quiz and question number - as in instructor's notes
    quizQuestion <- challengeLevel[row,itemIDCol2014]
    q <- str_locate(pattern="q",quizQuestion)
    quizString <- str_locate(pattern="Quiz",quizQuestion)
    quiz <- substr(quizQuestion,quizString+4,q-2)
    question <- substr(quizQuestion,q+1,str_length(quizQuestion))
    
    # add quiz and question info to data frame
    thisQuestion2014$quiz_14 <- quiz
    thisQuestion2014$question_14 <- question
    
    
    # Extra step here: 
    # correct for adding two non-content question at the beginning in year 2
    # which means "q1" has become "q3" etc. 
    # not elegant, but this is a quick fix for now
    question <- as.numeric(question)+2  
    
    
    # get easiness and time
    questionCol <- paste("Q",quiz,"q",question,sep="")
    thisQuestion2014$easiness_14 <- year2easiness[questionCol]
    thisQuestion2014$time_14 <- year2time[questionCol]
    
    # get challenge level
    thisQuestion2014$level_14 <- challengeLevel[row,ratingCol2014]
    
    # add to 2013 dataframe
    allQuestions2014 <- rbind(allQuestions2014,thisQuestion2014)
  }
    
  existsBoth <- exists2013*exists2014
  # finally, if both exist and are MC, add them to the combined data frame
  if(existsBoth == 1){
    
    thisQuestion <- cbind(thisQuestion2013,thisQuestion2014)
    allQuestionsBothYears <- rbind(allQuestionsBothYears,thisQuestion)
  }
  
}
 
# plot all
filename=paste(output_dir,"easiness_time_level_",year1,".png",sep="")
png(filename)
title=paste("Easiness, Time, Level (all quizzes), ",year1,sep="")
plot <- qplot(allQuestions2013$time_13,allQuestions2013$easiness_13,color=allQuestions2013$level_13) +
  scale_color_brewer(palette="Dark2", name="category")    +
  ggtitle(title) +
  xlab("Median time [min]") + 
  ylab("Easiness")  +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
  xlim(c(-0.2,6.2)) +
  ylim(c(0,1)) +
  geom_hline(yintercept=0.75) +
  geom_vline(xintercept=1) 
print(plot)
dev.off()

filename=paste(output_dir,"easiness_time_level_",year2,".png",sep="")
png(filename)
title=paste("Easiness, Time, Level (all quizzes), ",year2,sep="")
plot <- qplot(allQuestions2014$time_14,allQuestions2014$easiness_14,color=allQuestions2014$level_14) +
  scale_color_brewer(palette="Dark2", name="category")    +
  ggtitle(title) +
  xlab("Median time [min]") + 
  ylab("Easiness")  +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
  xlim(c(-0.2,6.2)) +
  ylim(c(0,1)) +
  geom_hline(yintercept=0.75) +
  geom_vline(xintercept=1) 
print(plot)
dev.off()

#### change and check all below

# plot per quiz
for (i in firstQuiz:lastQuiz){
    filename=paste(output_dir,"easiness_time_level_Q",i,"_",year1,".png",sep="")
    title=paste("Easiness, Time, Level, Quiz ",i,", ", year1, sep="")
    Questions <- allQuestions2013[allQuestions2013$quiz_13==i,]    
    png(filename)
    plot <- 
        qplot(Questions$time_13,Questions$easiness_13,color=Questions$level_13,
              label=Questions$question_13) +
        geom_text(size=5,hjust=-0.5)  +
        scale_color_brewer(palette="Dark2", name="category")    +
        ggtitle(title) +
        xlab("Median time [min]") + 
        ylab("Easiness")  +
        theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
        xlim(c(-0.2,6.2)) +
        ylim(c(0,1)) +
        geom_hline(yintercept=0.75) +
        geom_vline(xintercept=1) 
    
    print(plot)
    dev.off()
    
    filename=paste(output_dir,"easiness_time_level_Q",i,"_",year2,".png",sep="")
    title=paste("Easiness, Time, Level, Quiz ",i,", ", year2, sep="")
    Questions <- allQuestions2014[allQuestions2014$quiz_14==i,]    
    png(filename)
    plot <- 
        qplot(Questions$time_14,Questions$easiness_14,color=Questions$level_14,
              label=Questions$question_14) +
        geom_text(size=5,hjust=-0.5)  +
        scale_color_brewer(palette="Dark2", name="category")    +
        ggtitle(title) +
        xlab("Median time [min]") + 
        ylab("Easiness")  +
        theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
        xlim(c(-0.2,6.2)) +
        ylim(c(0,1)) +
        geom_hline(yintercept=0.75) +
        geom_vline(xintercept=1) 
    
    
    print(plot)
    dev.off()
}

# ## write xlsx
# write.xlsx(allQuestionsYear1, paste(output_dir,"time_level_easiness_2013.xlsx",sep=""), 
#            col.names = TRUE,row.names = FALSE,showNA = TRUE)
# write.xlsx(allQuestionsYear2, paste(output_dir,"time_level_easiness_2014.xlsx",sep=""), 
#            col.names = TRUE,row.names = FALSE,showNA = TRUE)
# 
# ## write csv
# write.csv(allQuestionsYear1, paste(output_dir,"time_level_easiness_2013.csv",sep=""),row.names=FALSE)
# write.csv(allQuestionsYear2, paste(output_dir,"time_level_easiness_2014.csv",sep=""),row.names=FALSE)
