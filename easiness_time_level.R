## easiness_time_level.R
## takes easiness data created with easiness_time.R
## takes time to answer each question created with order_time.R
## takes instructor-rated level of each question (1=low, 3=high)
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## Written by MI Stefan

## load necessary libraries
library(Hmisc)
library(xlsx)

# read in easiness and median time data 
easiness <- load(file="easinessPlot.Rda")
times <- load(file="medianTimesPlot.Rda")

# read in xls with cognitive level rating
cognitiveLevel <- read.xlsx("./Quiz2013-14_cognitive level_HB.xlsx",1)

# provide question column and rating column
qestionCol = 3
ratingCol = 5

# provide questions to ignore (can be empty), as determined by instructor
# here, we are ignoring question 12 of Quiz 9 (duplicate question)
ignore = "Q9_q12"

lowestQuiz = 4
highestQuiz = 32

# go through every quiz





