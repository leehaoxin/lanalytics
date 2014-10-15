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
timesToAnswer <- times[,1]
orders <- times[,1]

# per quiz
for (i in lowestQuiz:highestQuiz){    
    # get indices of relevant columns by finding pattern Q<quiz number>q
    indices = grep(paste("Q",i,"q",sep=""),colnames(times))
    currentQuiz = times[ ,indices]
    
    # will store times for this quiz
    quizTimeTaken <- currentQuiz
    quizTimeTaken[,] <- NA
    
    # will store orders for this quiz
    quizOrders <- data.frame()
    
    # go through each student
    
    for (j in 1:nrow(currentQuiz)){
        absoluteTimes = currentQuiz[j, ]
        
        # determine order
        questionOrder=order(absoluteTimes)
        
        # determine time to answer each question
                
#         for (k in 2:length(questionOrder)){
#             this <- as.numeric(questionOrder[k])
#             previous <- as.numeric(questionOrder[k-1])
#             timeTaken=absoluteTimes[this]-absoluteTimes[previous]
#             quizTimeTaken[j,this] <- timeTaken
#         }
        
        quizOrders[j,] <- questionOrder
    
        
    }
    # add to master list of orders
    cbind(orders,quizOrders)
    
    # add to master list of question times
    
    
}



# visualise orders (?)