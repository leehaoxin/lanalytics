## guessers.R
## takes item correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes proportion of students who guessed

## Written by MI Stefan

library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

# read in correctness and time data (inSeconds)
correctness <- read.csv(file="correctness.csv",sep=",")
# ignore first column (row indices)
correctness <- correctness[,2:ncol(correctness)]
load(file="./timesToAnswerSec.Rda")

# again, want to go from quiz 4 to quiz 32
lowestQuiz = 4
highestQuiz = 32

# make table that will hold cheating/guessing info
# convention: 
# - time under threshold and correct answer: cheating (1)
# - time under threshold and wrong anwer: guessing (-1)
# - time >= threshold or first two or last two questions: 0
# threshold is median time taken for first two and last two questions 
# (computed separately for each student and quiz)
cheating_guessing <- timesToAnswerSec
cheating_guessing[,2:ncol(cheating_guessing)] = 0


# make data frame to hold number of questions per quiz
questions_per_quiz <- data.frame(matrix(NA, nrow=highestQuiz-lowestQuiz+1,ncol=3))
colnames(questions_per_quiz) <- c('quiz','numberOfQuestions','cumulative')
cumulativeQuestions=0;


# make table that will hold all threshold times computed
# (in order to see their distribution, to sanity-check our approach)
thresholdList <- vector()

# for each quiz and student take median time of first two and last two questions
# see questions that were faster than that

masterIndex = 1

for (i in lowestQuiz:highestQuiz){
    # get indices of relevant columns by finding pattern Q<quiz number>q
    indices = grep(paste("Q",i,"q",sep=""),colnames(timesToAnswerSec))
    currentQuiz = timesToAnswerSec[ ,indices]
    lastQuestion = ncol(currentQuiz)
    
    # update table of question numbers
    cumulativeQuestions <- cumulativeQuestions + lastQuestion
    questions_per_quiz[i-lowestQuiz+1,"quiz"] <- i
    questions_per_quiz[i-lowestQuiz+1,"numberOfQuestions"] <- lastQuestion
    questions_per_quiz[i-lowestQuiz+1,"cumulative"] <- cumulativeQuestions
    
    # go through all students
    for (j in 1:nrow(currentQuiz)){
        learningQuestions <- currentQuiz[j,c(1,2,lastQuestion-1,lastQuestion)]    
        suppressWarnings(learningQuestions <- as.numeric(as.matrix(learningQuestions)))
        # threshold <- median(learningQuestions,na.rm=TRUE)
        suppressWarnings(threshold <- min(learningQuestions,na.rm=TRUE))
        # deal with special case of all learningQuestions being NA
        # (happens if they are not completed)
        if (threshold == Inf){
            threshold <- NA   
        }        
        
         # just to be on the safe side, cap threshold at 20 seconds
         if (is.na(threshold) || (threshold > 20)){
             threshold <- 20
         }
        
        thresholdList <- c(thresholdList,threshold)
        
        if(is.na(threshold)){
            next
        }
        
        for (k in 3:(lastQuestion-2)){
            suppressWarnings(qTime <- as.numeric(currentQuiz[j,k]))
            if ( !is.na(qTime) && (qTime < threshold)){
                if (is.null(correctness[j,masterIndex+k])){
                    next
                }
                else if (correctness[j,masterIndex+k] == 1){
                    cheating_guessing[j,masterIndex+k] = 1
                }   
                else if (correctness[j,masterIndex+k] ==0){
                    cheating_guessing[j,masterIndex+k] = -1
                }                        
                                        
            }                            
                        
        }
        
    }
    

    masterIndex <- masterIndex + ncol(currentQuiz)
}

png("thresholds_hist.png")
hist(thresholdList,100,xlab="threshold [s]",main="Guessing thresholds")
dev.off()

png("cheating_guessing.png",width = 1200, units = "px")
cheating_guessing_numbers <- as.matrix(cheating_guessing[,2:ncol(cheating_guessing)])
hmcol<-brewer.pal(3,"RdBu")
image(t(cheating_guessing_numbers), col=rev(hmcol), axes=FALSE, xlab="quiz", ylab="student")
tickmarks = (questions_per_quiz[,"cumulative"]-questions_per_quiz[1,"cumulative"])/cumulativeQuestions
axis(1, labels=lowestQuiz:highestQuiz,  at=tickmarks,cex.axis=1)
dev.off()



# firstquiz <- cheating_guessing[,2:19]
# firstquiz_numbers <- as.matrix(firstquiz)
# hmcol<-brewer.pal(3,"RdBu")
# image(t(firstquiz_numbers), col=rev(hmcol))