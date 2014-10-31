## order_time.R
## takes timing data created with parsequizzes.R
## computes order in which questions were answered and absolute time taken
## Written by MI Stefan

# give number of quizzes we are interested in 
lowestQuiz = 4
highestQuiz = 32

# read in time data
times <- read.csv(file="times.csv",sep="\t",stringsAsFactors=FALSE)

# data frames that will hold orders of questions and times to answer each
timesToAnswerMin <- as.data.frame(times[,1])
timesToAnswerSec <- as.data.frame(times[,1])
orders <- as.data.frame(times[,1])

# per quiz
for (i in lowestQuiz:highestQuiz){
     # get indices of relevant columns by finding pattern Q<quiz number>q
     indices = grep(paste("Q",i,"q",sep=""),colnames(times))
     currentQuiz = times[ ,indices]
     
     # will store times for this quiz
     quizTimeTakenMin <- currentQuiz
     quizTimeTakenMin[,] <- NA
     
     quizTimeTakenSec <- currentQuiz
     quizTimeTakenSec[,] <- NA
     
     # will store orders for this quiz
     quizOrders <- data.frame()
     
     # go through each student
     
     for (j in 1:nrow(currentQuiz)){
         absoluteTimes = currentQuiz[j, ]
         absoluteTimes <- strptime(absoluteTimes, "%Y-%m-%d %H:%M:%S")
                  
         # determine order, e.g. if the first entry in questionOrder is 3, 
         # this means question 3 was answered first
         questionOrder=order(absoluteTimes)
         
        # determine time to answer each question (in minutes and in seconds)
        column_names<-list()
        
        for (k in 2:length(questionOrder)){
        this <- questionOrder[k]
        previous <- questionOrder[k-1]
        timeTakenMin=as.numeric(difftime(absoluteTimes[this], absoluteTimes[previous], units="mins"))          
        quizTimeTakenMin[j,this] <- format(round(timeTakenMin,0),)
        timeTakenSec=as.numeric(difftime(absoluteTimes[this], absoluteTimes[previous], units="secs"))          
        quizTimeTakenSec[j,this] <- format(round(timeTakenSec,0),)
        column_names = c(column_names, paste('Q',i,'_',(k-1),'th',sep=""))                    
        }
        column_names = c(column_names, paste('Q',i,'_',(k),'th',sep=""))
        
        quizOrders <- rbind(quizOrders, questionOrder)
        colnames(quizOrders) <- column_names 
     }
     # add to master list of orders
     orders <- cbind(orders,quizOrders)
     timesToAnswerMin <- cbind(timesToAnswerMin,quizTimeTakenMin)
     timesToAnswerSec <- cbind(timesToAnswerSec,quizTimeTakenSec)
     
     # add to master list of question times
     
     
}

# save data frames

save(orders,file="orders.Rda")
save(timesToAnswerMin,file="timesToAnswerMin.Rda")
save(timesToAnswerSec,file="timesToAnswerSec.Rda")
