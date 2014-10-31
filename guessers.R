## guessers.R
## takes item correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes proportion of students who guessed

## Written by MI Stefan

# read in correctness and time data (inSeconds)
correctness <- read.csv(file="correctness.csv",sep="\t")
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

# for each quiz and student take median time of first two and last two questions
# see questions that were faster than that

for (i in lowestQuiz:highestQuiz){
    # get indices of relevant columns by finding pattern Q<quiz number>q
    indices = grep(paste("Q",i,"q",sep=""),colnames(timesToAnswerSec))
    currentQuiz = timesToAnswerSec[ ,indices]
    lastQuestion = ncol(currentQuiz)
    # go through all students
    for (j in 1:nrow(currentQuiz)){
    learningQuestions <- currentQuiz[j,c(2,3,lastQuestion-1,lastQuestion)]    
    learningQuestions <- as.numeric(as.matrix(learningQuestions))
    threshold <- median(learningQuestions,na.rm=TRUE)        
        
    }
}