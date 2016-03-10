# item_discrimination.R
# comuptes item discrimination for quizzes 
# reports mean item discrimination and standard deviation 
# tried to use psychometric library, but ran into trouble
# so this is a new implementation of item discrimination
# (score (top group) - score (bottom group))/(group size)
# with group size being around 27% of the entire class
# (http://ericae.net/ft/tamu/Espy.htm)  

# written by MI Stefan

library(xlsx) # read xlsx files
library(plyr) # allows for sorting of data frames

# read in data 
correctness <- read.csv("2013_output/correctness.csv")

# # read in data from both years
# correctness2014 <- read.xlsx("correctness.xlsx",1,as.data.frame=TRUE)




#  
# remove first columns (holding row numbers)
correctness[,1] <- NULL

# compute total score per student and add this as the last column to the data frame
correctness[,"total"] <- rowSums(correctness[,2:ncol(correctness)],na.rm=TRUE)

# sort data frame by total number of answers correct 
sortedCorrectness <- arrange(correctness,total)

# compute number of students in upper and lower group 

numberOfStudents <- nrow(correctness)
groupSize <- round(numberOfStudents*27/100)

# make a bottom group and a top group by selecting the first and last groupSize students
bottomGroup <- sortedCorrectness[1:groupSize,]
topGroup <- sortedCorrectness[(numberOfStudents-groupSize+1):numberOfStudents,]

# make new data frame to hold item discrimination for each item
itemDiscrimination <- as.data.frame(matrix(nrow = 0, ncol = ncol(correctness)))
names(itemDiscrimination)=names(correctness)
itemDiscrimination[,1]<-NULL

# now comes the actual item discrimination computing

 
for (i in 2:ncol(correctness)){
  topScore <- sum(topGroup[,i])
  bottomScore <- sum(bottomGroup[,i])
  itemDiscrimination <- (topScore-bottomScore)/groupSize
  # itemDiscrimination[1,i] <- itemDiscrimination
}




