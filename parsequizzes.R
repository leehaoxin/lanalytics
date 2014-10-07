## parsequizzes.R
## parses data from all quizzes in a given directory into three separate tables
## Written by MI Stefan

library(stringr)
library(xlsx)

# get input directory 
quizdir = ("/home/melanie/Dropbox/Academy Project/Raw Data 2014/")

# make 3 master tables: responses, item correctness (1 or 0), time
responses <- data.frame()
correctnes <- data.frame()
times <- data.frame()

# read in all files from input directory
filenames <- list.files(quizdir, pattern="*.xlsx")


# go through every file
# for (i in 1:length(filenames)){
for (i in 1:2){
    
    thisquiz = filenames[i]
    
    # extract quiz number from file name
    # This depends on the quiz naming scheme!
    # in our case, the quiz Number always begins after the year (2014),
    # and there is an underscore after it
    yearnumber =  sub("_(.+)","",thisquiz)
    quiznumber = sub("2014","",yearnumber)
        
    # read in xlsx file
    file = paste(quizdir,thisquiz,sep="")
    quizdata = read.xlsx(file,1, as.data.frame=TRUE)
            
    # go through every student
    for (j in 1:nrow(quizdata)){
        # look up student identifier and see if it's alreaday in tables
        # if not, create new entry
        
        
        
        # go through every column
        
        # make question name according to schema
        
        # add entry to appropriate table
        
        # add response given to response table
        
        # add item correctness to item response table
        
        # add absolute time to times table        
        
    }
        
}



## save three tables to .xls files




 





