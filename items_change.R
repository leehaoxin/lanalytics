## items_change.R
## using data from two successive offerings of the course
## plots and quantifies how items have changed from one year to the next
## Written by MI Stefan

compare_2013_2014 <- read.csv("2013_2014_compare/time_level_easiness_2013_2014.csv")

## plot the "star of change" for all questions
## prediction: if nothing happens, star should be the same in all directions
## if there is improvement, star should go towards the top left (?)

plot(c(-4,4),c(-0.5,0.5),xlab="time change", ylab="easiness change")

min_easiness_change = 0

for (i in 1:nrow(compare_2013_2014)){
  easiness_change = compare_2013_2014[i,"easiness_14"]-compare_2013_2014[i,"easiness_13"]
  print (easiness_change)
  time_change = compare_2013_2014[i,"time_14"]-compare_2013_2014[i,"time_13"]
    
  segments(0, 0, time_change, easiness_change)
}

# plot <- qplot(allQuestions2013$time_13,allQuestions2013$easiness_13,color=allQuestions2013$level_13) +
#   scale_color_brewer(palette="Dark2", name="category")    +
#   ggtitle(title) +
#   
#   segments(0, 0, easiness_change, time_change)
# 
# 
# xlim(-4,4)
# ylim(-1,)