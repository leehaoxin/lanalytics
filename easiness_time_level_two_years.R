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

# check data set
head(challengeLevel)
names(challengeLevel)
                            

# create empty data frame 
# holds easiness, median Time, challenge level, quiz number, question numrber
allQuestionsYear1 <- data.frame(matrix(nrow=0,ncol=5))
allQuestionsYear2 <- data.frame(matrix(nrow=0,ncol=5))

# provide question column and rating column both for 2013 and 2014

# questionCol = 3
# ratingCol = 5
# 
# 
# ######### deal with exceptions/special cases
# 
# # provide questions to ignore (can be empty), as determined by instructor
# # here, we are ignoring question 12 of Quiz 9 (duplicate question)
# ignore = "Q9_q12"
# 
# ## question 1 and 4 were swapped in Quiz 4 from 2013 to 2014. 
# ## Add star in plot
# star = c("Q4_q1","Q4_q4")
# 
# ######### start analysis
# 
# lowestQuiz=4
# highestQuiz=32
# 
# # go through all question for which we have a time and easiness rating
# for (i in 1:length(names(year1time))){
#     
#     name = names(year1time)[i]
#             
#     # make empty dataframe to hold all info for this question
#     thisQuestionYear1 <- data.frame(matrix(nrow=1,ncol=5))
#     thisQuestionYear2 <- data.frame(matrix(nrow=1,ncol=5))
#     
#     colnames(thisQuestionYear1) <- c("easiness","time", "level", "quiz","question")
#     colnames(thisQuestionYear2) <- c("easiness","time", "level", "quiz","question")
#     
#     # get quiz and question number 
#     quizQuestion <- names(year1time)[i]
#     q <- str_locate(pattern="q",quizQuestion)
#     quiz <- substr(quizQuestion,2,q-1)
#     question <- substr(quizQuestion,q+1,str_length(quizQuestion))
#     
#     # correct for adding two non-content question at the beginning in year 2
#     # which means "q1" has become "q3" etc. 
#     # not elegant, but this is a quick fix for now
#     year2question <- as.numeric(question)+2
#     year2name <- paste("Q",quiz,"q",year2question,sep="")
#     
#     # check that this quiz question exists in year 2
#     if (!is.na(year2easiness[year2name])){
#         ## fill in data
#         thisQuestionYear1$quiz <- quiz
#         thisQuestionYear1$question <- question
#         thisQuestionYear2$quiz <- quiz
#         thisQuestionYear2$question <- question
#         
#         # get easiness and time
#         thisQuestionYear1$easiness <- year1easiness[name]
#         thisQuestionYear1$time <- year1time[name]
#         thisQuestionYear2$easiness <- year2easiness[year2name]
#         thisQuestionYear2$time <- year2time[year2name]
#                 
#         # find this quiz and question number (with underscore)
#         
#         questionString = paste("Q",quiz,"_q",question,sep="")
#         if (ignore != questionString){        
#             cogIndex <- which(challengeLevel[,questionCol]==questionString)
#             if (length(cogIndex >0)){
#                 level <- challengeLevel[cogIndex,ratingCol]
#                 thisQuestionYear1$level <- level
#                 thisQuestionYear2$level <- level
#                 allQuestionsYear1 <- rbind(allQuestionsYear1,thisQuestionYear1)
#                 allQuestionsYear2 <- rbind(allQuestionsYear2,thisQuestionYear2)
#                 
#             }
#         }   
#         
#     }    
# }
#     
# 
# allQuestionsYear1$level <- as.factor(allQuestionsYear1$level)
# allQuestionsYear2$level <- as.factor(allQuestionsYear2$level)
# 
# 
# # plot all
# filename=paste(output_dir,"easiness_time_level_",year1,".png",sep="")
# png(filename)
# title=paste("Easiness, Time, Level (all quizzes), ",year1,sep="")
# plot <- qplot(allQuestionsYear1$time,allQuestionsYear1$easiness,color=allQuestionsYear1$level) +
#     scale_color_brewer(palette="Dark2", name="challenge Level")    +
#     ggtitle(title) +
#     xlab("Median time [min]") + 
#     ylab("Easiness [%]")  +
#     theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
#     xlim(c(-0.2,6.2)) +
#     ylim(c(0,1))
# print(plot)
# dev.off()
#     
# filename=paste(output_dir,"easiness_time_level_",year2,".png",sep="")
# png(filename)
# title=paste("Easiness, Time, Level (all quizzes), ",year2,sep="")
# plot <- qplot(allQuestionsYear2$time,allQuestionsYear2$easiness,color=allQuestionsYear2$level) +
#     scale_color_brewer(palette="Dark2", name="challenge Level")    +
#     ggtitle(title) +
#     xlab("Median time [min]") + 
#     ylab("Easiness [%]")  +
#     theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
#     xlim(c(-0.2,6.2)) +
#     ylim(c(0,1))
# print(plot)
# dev.off()
# 
# 
# 
# # plot per quiz
# for (i in lowestQuiz:highestQuiz){
#     filename=paste(output_dir,"easiness_time_level_Q",i,"_",year1,".png",sep="")
#     title=paste("Easiness, Time, Level, Quiz ",i,", ", year1, sep="")
#     Questions <- allQuestionsYear1[allQuestionsYear1$quiz==i,]    
#     png(filename)
#     plot <- 
#         qplot(Questions$time,Questions$easiness,color=Questions$level,
#               label=Questions$question) +
#         geom_text(size=5,hjust=-0.5)  +
#         scale_color_brewer(palette="Dark2", name="challenge Level")    +
#         ggtitle(title) +
#         xlab("Median time [min]") + 
#         ylab("Easiness [%]")  +
#         theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
#         xlim(c(-0.2,6.2)) +
#         ylim(c(0,1))
#     
#     print(plot)
#     dev.off()
#     
#     filename=paste(output_dir,"easiness_time_level_Q",i,"_",year2,".png",sep="")
#     title=paste("Easiness, Time, Level, Quiz ",i,", ", year2, sep="")
#     Questions <- allQuestionsYear2[allQuestionsYear2$quiz==i,]    
#     png(filename)
#     plot <- 
#         qplot(Questions$time,Questions$easiness,color=Questions$level,
#               label=Questions$question) +
#         geom_text(size=5,hjust=-0.5)  +
#         scale_color_brewer(palette="Dark2", name="challenge Level")    +
#         ggtitle(title) +
#         xlab("Median time [min]") + 
#         ylab("Easiness [%]")  +
#         theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
#         xlim(c(-0.2,6.2)) +
#         ylim(c(0,1))
#     
#     print(plot)
#     dev.off()
#     
#     
# 
# }
# 
# ## write xlsx
# write.xlsx(allQuestionsYear1, paste(output_dir,"time_level_easiness_2013.xlsx",sep=""), 
#            col.names = TRUE,row.names = FALSE,showNA = TRUE)
# write.xlsx(allQuestionsYear2, paste(output_dir,"time_level_easiness_2014.xlsx",sep=""), 
#            col.names = TRUE,row.names = FALSE,showNA = TRUE)
# 
# ## write csv
# write.csv(allQuestionsYear1, paste(output_dir,"time_level_easiness_2013.csv",sep=""),row.names=FALSE)
# write.csv(allQuestionsYear2, paste(output_dir,"time_level_easiness_2014.csv",sep=""),row.names=FALSE)
