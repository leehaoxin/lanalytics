## difficulty_time.R
## takes timing and item response data created with parsequizzes.R
## creates scatterplot of item difficulty vs time
## Written by MI Stefan

# read in correctness and time data
correctness <- read.csv(file="correctness.csv",sep="\t")
times <- read.csv(file="times.csv",sep="\t")

# get rate of correct answers
rateCorrect = colMeans(correctness[,2:length(correctness)],na.rm = TRUE)

# get median time it took to answer each question
# timeCorrect = apply(X = times[,2:length(times),2,FUN = median, na.rm=TRUE])
# TODO: Actually write function that computes times to answer first!