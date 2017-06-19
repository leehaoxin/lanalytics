## easiness_time.R
## takes item correctness data created with parsequizzes.R
## takes time to answer each question created with order_time.R
## computes item easiness and median answer time per question
## creates scatterplot of item easiness vs time
## Written by MI Stefan

## load necessary libraries
library(Hmisc)
library(xlsx)

# specify output directory
output_dir="2013_output/"

# read in correctness and time data (inSeconds)
correctness_file <- paste(output_dir,"correctness.csv",sep="")
correctness <- read.csv(file=correctness_file,sep=",")
timesToAnswerSec_file <- paste(output_dir,"timesToAnswerSec.Rda",sep="")
load(file=timesToAnswerSec_file)

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
time_easiness_correlatoin_matrix_file <- paste(output_dir,"time_easiness_correlation_matrix.xlsx",sep="")
write.xlsx(correlation$r, file=time_easiness_correlatoin_matrix_file)
time_easiness_correlation_pvalues_file <- paste(output_dir,"time_easiness_correlation_pvalues.xlsx",sep="")
write.xlsx(correlation$P, file=time_easiness_correlation_pvalues_file)

r = format(correlation$r[2],digits=2)
P = format(correlation$P[2],digits=2)
    
labeltext <- paste("r = ",r,"\nP = ", P,sep="")


# scatter plot of easiness v time, plus linear regression
easiness_time_file <- paste(output_dir,"easiness_time.pdf",sep="")
pdf(easiness_time_file)
plot(medianTimesPlot,rateCorrectPlot,xlab="median time [min]",ylab="easiness [%]",
     col=rgb(0,100,0,50,maxColorValue=255), pch=16, ylim=c(0,1.02), 
     main="Easiness and time per question")
reg <- lm(rateCorrectPlot~medianTimesPlot)
abline(reg)
legend('bottomright', legend=labeltext)
dev.off()
 
# write easiness and median time data to .xlsx
easiness_file <- paste(output_dir,"easiness.xlsx",sep="")
write.xlsx(t(rateCorrect),file = easiness_file, col.names = TRUE,row.names = FALSE,showNA=TRUE)
median_times_min_file <- paste(output_dir,"median_times_min.xlsx",sep="")
write.xlsx(medianTimesMin,file = median_times_min_file, col.names = TRUE,row.names = FALSE,showNA=TRUE)
 
# save plotted easiness and median time data
easinessPlot_file <- paste(output_dir,"easinessPlot.Rda",sep="")
save(rateCorrectPlot,file=easinessPlot_file)
names(medianTimesPlot) <- names(rateCorrectPlot)
medianTimesPlot_file <- paste(output_dir,"medianTimesPlot.Rda",sep="")
save(medianTimesPlot,file=medianTimesPlot_file)
