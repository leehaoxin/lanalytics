# median_response_times_two_years.R
# compute median of all response times across all students for both years (2013 and 2014)

# # load 2013 data; convert to matrix; remove NA
load(file="2013_output/timesToAnswerSec.Rda")

# ignore first column; make into numeric; linearize
timesToAnswerSec2013 <- timesToAnswerSec[,2:ncol(timesToAnswerSec)]
timesToAnswerSec2013 <- lapply(timesToAnswerSec2013,as.numeric)
timesToAnswerSec2013 <- unlist(timesToAnswerSec2013)

# load 2014 data; convert to matrix
load("timesToAnswerSec.Rda")
timesToAnswerSec2014 <- as.matrix(timesToAnswerSec[2:nrow(timesToAnswerSec),])
# ignore first column; make into numeric; linearize
timesToAnswerSec2014 <- timesToAnswerSec[,2:ncol(timesToAnswerSec)]
timesToAnswerSec2014 <- lapply(timesToAnswerSec2014,as.numeric)
timesToAnswerSec2014 <- unlist(timesToAnswerSec2014)

allTimesToAnswerSec <- c(timesToAnswerSec2013,timesToAnswerSec2014)
 
# medianTimeToAnswerSec2013 <- median(timesToAnswerSec2013,na.rm=TRUE)
# medianTimeToAnswerSec2014 <- median(timesToAnswerSec2014,na.rm=TRUE)
# medianTimeToAnswerSecAll <- median(allTimesToAnswerSec,na.rm=TRUE)
# 
