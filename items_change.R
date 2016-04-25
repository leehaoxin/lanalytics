## items_change.R
## using data from two successive offerings of the course
## plots and quantifies how items have changed from one year to the next
## Written by MI Stefan

compare_2013_2014_raw <- read.csv("2013_2014_compare/time_level_easiness_2013_2014.csv")

## get rid of all first questions in 2013 (time readout not trustworthy)
compare_2013_2014 <- compare_2013_2014_raw[compare_2013_2014_raw[,'question_13']!=1,]

## plot the "star of change" for all questions
## prediction: if nothing happens, star should be the same in all directions
## if there is improvement, star should go towards the top left (?)

plot(NA, xlim=c(-4,4), ylim=c(-0.5,0.5),xlab="time change", ylab="easiness change", 
     main="Item changes 2013-2014")

min_easiness_change = 0

for (i in 1:nrow(compare_2013_2014)){
  easiness_change = compare_2013_2014[i,"easiness_14"]-compare_2013_2014[i,"easiness_13"]
  time_change = compare_2013_2014[i,"time_14"]-compare_2013_2014[i,"time_13"]
  compare_2013_2014[i,"easiness_change"] <- easiness_change
  compare_2013_2014[i,"time_change"] <- time_change
  segments(0, 0, time_change, easiness_change)
}


