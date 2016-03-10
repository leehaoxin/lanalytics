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
correctness2013 <- read.csv("2013_output/correctness.csv")

# # read in data from both years
# correctness2014 <- read.xlsx("correctness.xlsx",1,as.data.frame=TRUE)




#  
# remove first columns (holding row numbers)
correctness2013[,1] <- NULL

# compute total score per student and add this as the last column to the data frame
correctness2013[,"total"] <- rowSums(correctness2013[,2:ncol(correctness2013)],na.rm=TRUE)

# sort data frame by total number of answers correct 
sortedCorrectness2013 <- arrange(correctness2013,total)

# compute number of students in upper and lower group 

numberOfStudents2013 <- nrow(correctness2013)
groupSize2013 <- round(numberOfStudents2013*27/100)

# make a bottom group and a top group by selecting the first and last groupSize students
bottomGroup2013 <- sortedCorrectness2013[1:groupSize2013,]
topGroup2013 <- sortedCorrectness2013[(numberOfStudents2013-groupSize2013+1):numberOfStudents2013,]

# make new data frame to hold item discrimination for each item
itemDiscrimination2013 <- as.data.frame(matrix(nrow = 0, ncol = ncol(correctness2013)))
names(itemDiscrimination2013)=names(correctness2013)
itemDiscrimination2013[,1]<-NULL

# now comes the actual item discrimination computing

 
for (i in 2:ncol(correctness2013)){
  topScore <- sum(topGroup[,i])
  bottomScore <- sum(bottomGroup[,i])
  itemDiscrimination <- (topScore-bottomScore)/groupSize2013
  # itemDiscrimination2013[1,i] <- itemDiscrimination
}




