# # read in easiness and median time data 
# load(file="2013_output/easinessPlot.Rda")
# load(file="2013_output/medianTimesPlot.Rda")
# 
# df_test %>% head
# challengeLevel <- read.xlsx("data/Quiz2013-14_cognitive level_HB.xlsx",1)
# lowestQuiz=1
# highestQuiz=3
# 
# for (i in 1:length(names(medianTimesPlot))){
#   # make empty dataframe to hold all info for this question
#   thisQuestion <- data.frame(matrix(nrow=1,ncol=5))
#   colnames(thisQuestion) <- c("easiness","time", "level", "quiz","question")
#   
#   # get easiness and time
#   thisQuestion$easiness <- rateCorrectPlot[i]
#   thisQuestion$time <- medianTimesPlot[i]
#   
#   # get quiz and question number 
#   quizQuestion <- names(medianTimesPlot)[i]
#   q <- str_locate(pattern="q",quizQuestion)
#   quiz <- substr(quizQuestion,2,q-1)
#   thisQuestion$quiz <- quiz
#   question <- substr(quizQuestion,q+1,str_length(quizQuestion))
#   ## correct for the file that the question apparing as "q3" in Learning Catalytics
#   ## is actually q1 in our internal files
#   thisQuestion$question <- as.numeric(question)
#   
#   # correct for instructor mistake in question naming 
#   # consistently wrote "q1" instead of "q3" etc. 
#   # this should not have happened, but this is a quick fix for now
#   question <- as.numeric(question)-2
#   
#   # find this quiz and question number (with underscore)
#   
#   questionString = paste("Q",quiz,"_q",question,sep="")
#   if (ignore != questionString){        
#     cogIndex <- which(challengeLevel[,questionCol]==questionString)
#     if (length(cogIndex >0)){
#       level <- challengeLevel[cogIndex,ratingCol]
#       thisQuestion$level <- level
#       allQuestions <- rbind(allQuestions,thisQuestion)
#       
#     }
#   }
# }
# 
# 
# allQuestions$level <- as.factor(allQuestions$level)
# 
# # plot all
# png("easiness_time_level.png")
# plot <- qplot(allQuestions$time,allQuestions$easiness,color=allQuestions$level) +
#   scale_color_brewer(palette="Dark2", name="challenge Level")    +
#   ggtitle("Easiness, Time, Level (all quizzes)") +
#   xlab("Median time [min]") + 
#   ylab("Easiness [%]")  +
#   theme(plot.title = element_text(size=20, face="bold", vjust=2))
# print(plot)
# dev.off()
# 
# # plot per quiz
# for (i in lowestQuiz:highestQuiz){
#   filename=paste("easiness_time_level_Q",i,".png",sep="")
#   title=paste("Easiness, Time, Level, Quiz ",i,sep="")
#   Questions <- allQuestions[allQuestions$quiz==i,]    
#   png(filename)
#   plot <- 
#     qplot(Questions$time,Questions$easiness,color=Questions$level,
#           label=Questions$question) +
#     geom_text(size=5,hjust=-0.5)  +
#     scale_color_brewer(palette="Dark2", name="challenge Level")    +
#     ggtitle(title) +
#     xlab("Median time [min]") + 
#     ylab("Easiness [%]")  +
#     theme(plot.title = element_text(size=20, face="bold", vjust=2))
#   
#   print(plot)
#   dev.off()
# }
# 
# ## write xlsx
# write.xlsx(allQuestions, "time_level_easiness_2014_only.xlsx", 
#            col.names = TRUE,row.names = FALSE,showNA = TRUE)
# 
# ## write csv
# write.csv(allQuestions, "time_level_easiness_2014_only.csv",row.names=FALSE)
# 
# 
# 
# 
# 
# 
# 
