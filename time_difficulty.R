## time_difficulty.R
## takes correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes item difficulty and median time for each question
## correlates them
## Written by MI Stefan

# read in correctness data
correctness <- read.csv(file="correctness.csv",sep="\t",stringsAsFactors=FALSE)

# load timing data (in minutes)
load(file="./timesToAnswerMin.Rda")

item_difficulty = data.frame(matrix(NA,nrow=1,ncol=0))

## loop through every quiz, compute item difficulty as mean of correct responses
for (i in 2:ncol(correctness)){
    difficulty <- mean(correctness[,i],na.rm=TRUE)
    item_difficulty <- cbind(item_difficulty,difficulty)
}

colnames(item_difficulty) <- colnames(correctness)[2:ncol(correctness)]
