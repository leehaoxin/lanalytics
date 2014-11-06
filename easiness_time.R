## easiness_time.R
## takes item correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## Written by MI Stefan

# read in correctness and time data (inSeconds)
correctness <- read.csv(file="correctness.csv",sep="\t")
load(file="./timesToAnswerSec.Rda")

# get rate of correct answers
rateCorrect = colMeans(correctness[,2:length(correctness)],na.rm = TRUE)

# get median time it took to answer each question
medianTimes <- data.frame(matrix(NA,nrow=1))
alltimes <- timesToAnswerSec[,2:length(timesToAnswerSec)]
for (i in 1:ncol(alltimes)){
    times <- as.matrix(alltimes[,i])
    suppressWarnings(qMedian <- median(as.numeric(times), na.rm=TRUE))
    medianTimes <- cbind(medianTimes,qMedian)
}

medianTimes <- as.matrix(medianTimes[2:length(medianTimes)])

pdf("easiness_time.pdf")
plot(medianTimes,rateCorrect,xlab="time [s]",ylab="easiness [%]",
     col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

