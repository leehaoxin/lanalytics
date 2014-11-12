## easiness_time.R
## takes item correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## Written by MI Stefan

## load necessary libraries
library(Hmisc)
library(xlsx)


# read in correctness and time data (inSeconds)
correctness <- read.csv(file="correctness.csv",sep=",")
load(file="./timesToAnswerSec.Rda")

# get rate of correct answers
rateCorrect = colMeans(correctness[,3:length(correctness)],na.rm = TRUE)

# get median time it took to answer each question
medianTimes <- data.frame(matrix(NA,nrow=1))
alltimes <- timesToAnswerSec[,2:length(timesToAnswerSec)]
for (i in 1:ncol(alltimes)){
    times <- as.matrix(alltimes[,i])
    suppressWarnings(qMedian <- median(as.numeric(times), na.rm=TRUE))
    medianTimes <- cbind(medianTimes,qMedian)
}

medianTimes <- as.matrix(medianTimes[2:length(medianTimes)])
medianTimesMin <- medianTimes/60

colnames(medianTimesMin) <- names(rateCorrect)

# will plot only questions with easiness > 0
# (the others are questions where there is no right or wrong)
rateCorrectPlot <- rateCorrect[rateCorrect>0]
medianTimesPlot <- medianTimesMin[rateCorrect>0]

# compute regression
correlation <- rcorr(rateCorrectPlot,medianTimesPlot)
write.xlsx(correlation$r, file="time_easiness_correlation_matrix.xlsx")
write.xlsx(correlation$P, file="time_easiness_correlation_pvalues.xlsx")

r = format(correlation$r[2],digits=2)
P = format(correlation$P[2],digits=2)
    
labeltext <- paste("r = ",r,"\nP = ", P,sep="")


# scatter plot of easiness v time, plus linear regression
pdf("easiness_time.pdf")
plot(medianTimesPlot,rateCorrectPlot,xlab="median time [min]",ylab="easiness [%]",
     col=rgb(0,100,0,50,maxColorValue=255), pch=16, ylim=c(0,1.02), 
     main="Easiness and time per question")
reg <- lm(rateCorrectPlot~medianTimesPlot)
abline(reg)
legend('bottomright', legend=labeltext)
dev.off()
 
# write easiness and median time data to .xlsx
 write.xlsx(t(rateCorrect),file = "easiness.xlsx", col.names = TRUE,row.names = FALSE,showNA=TRUE)
 write.xlsx(medianTimesMin,file = "median_times_min.xlsx", col.names = TRUE,row.names = FALSE,showNA=TRUE)
 
# save plotted easiness and median time data
save(rateCorrectPlot,file="easinessPlot.Rda")
names(medianTimesPlot) <- names(rateCorrectPlot)
save(medianTimesPlot,file="medianTimesPlot.Rda")
