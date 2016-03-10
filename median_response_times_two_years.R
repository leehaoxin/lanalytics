# median_response_times_two_years.R
# compute median of all response times across all students for both years (2013 and 2014)

# # load 2013 data; convert to matrix
load(file="2013_output/timesToAnswerSec.Rda")
timesToAnswerSec2013 <- as.matrix(timesToAnswerSec)

# load 2014 data; convert to matrix
load("timesToAnswerSec.Rda")
timesToAnswerSec2014 <- as.matrix(timesToAnswerSec)

allTimesToAnswer <- c(timesToAnswerSec2013,timesToAnswerSec2014)


