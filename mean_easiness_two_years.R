## takes easiness data created using easiness_time.R
## computes averages of item easiness 2 years
## Written by MI Stefan

library(xlsx)  # reading of xlsx files


## import easiness data from both years
easiness2013 <- read.xlsx("2013_output/easiness.xlsx",1)
easiness2014 <- read.xlsx("easiness.xlsx",1)

## convert to matrices
easiness2013 <- as.matrix(easiness2013)
easiness2014 <- as.matrix(easiness2014)

## select only questions with easiness > 0 (where there was a right or wrong answer)
easinessRW2013 <- easiness2013[easiness2013>0]
easinessRW2014 <- easiness2014[easiness2014>0]
easinessRW <- c(easiness2013,easiness2014)

## compute mean and standard deviation 
mean_easiness_2013 <- mean(easinessRW2013,na.rm=TRUE)
mean_easiness_2014 <- mean(easinessRW2014,na.rm=TRUE)
mean_easiness <- mean(easinessRW)

std_easiness_2013 <- sd(easinessRW2013,na.rm=TRUE)
std_easiness_2014 <- sd(easinessRW2014,na.rm=TRUE)
std_easiness <- sd(easinessRW)



