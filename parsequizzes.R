## parsequizzes.R
## parses data from all quizzes in a given directory into three separate tables
## Written by MI Stefan

library(stringr)
library(xlsx)
library(digest)
library(random)

## Variables (to be specified according to project)
# input directory
quizdir = ("/home/melanie/Work/MCM/academy_project/Raw data 2013/Quizzes 4-32 2013 copy")
# year quiz was administered (important for date format conversions later on)
quizyear = 2013
# number of students
studentNumber = 176;
# number of first and last quiz (in case it does not start with 1)
firstquiz=4;
lastquiz=32;

# make 3 master tables: responses, item correctness (1 or 0), time
responses <- data.frame()
correctness <- data.frame()
times <- data.frame()

# make table to hold student names and IDs (careful about who gets this)
students <- data.frame()


# read in all files from input directory
filenames <- list.files(quizdir, pattern="*.xlsx")

# create random 4-number ids for students; 
# need to know number of students for that

studentIDs <- sample(1000:9999, studentNumber, replace = F)

# go through every quiz (need to know number of lowest and highest quiz)
for (i in firstquiz:lastquiz){
    # read in xlsx file - this assumes file names are of the form Quiz3_something.xlsx
    fileindex <- grep(paste("Quiz",i,"_",sep=""),filenames[])
    thisquiz <- filenames[fileindex]
    file = paste(quizdir,thisquiz,sep="")
    quizdata = read.xlsx(file,1, as.data.frame=TRUE) 
    
             
     # go through every student
     for (j in 1:nrow(quizdata)){
         # get student e-mail
         mail = as.character(quizdata[j,1])
         # use first quiz to make table of students and id
         if (i == firstquiz){
             id <- studentIDs[j]
             students[j,"id"] <- id
             students[j,"mail"] <- mail
             responses[j,"id"] <- id            
             correctness[j,"id"] <- id
             times[j,"id"] <- id
             index = j
         }
         # for all other quizzes, look up student e-mail in table to get id
         else{ 
             # look up id in students list
             id <- students[(students[,2]==mail),1]
             index = which(responses[,1]==id)         
         }
                              
        # go through every column (after the name)
        for (m in 2:ncol(quizdata)){
            column = colnames(quizdata)[m]
            if (length(grep("Question.*", column))>0){
                # make question name according to schema
                question = substring(column, 10,11)
                question = (sub("[^0-9]","",question))
                quizquestion = paste("Q",i,"q",question,sep="")
                
                # add entry to appropriate table:
                                
                # add response given to response table
                if(length(grep(".*response.*",column))>0){
                    qresponse  = quizdata[j,m]
                    # strip response of \n and \t
                    qresponse = gsub("\n","",qresponse)
                    qresponse = gsub("\t","",qresponse)
                    # get rid of double quotes 
                    qresponse = gsub("\"","\'",qresponse)
                    responses[index, quizquestion] = qresponse                    
                }
                # add item correctness to item response table
                else if(length(grep(".*score.*",column))>0){
                    correctness[index, quizquestion] = quizdata[j,m]                              
                }                
                # add absolute time to times table                                
                else if(length(grep(".*responded.*",column))>0){
                    atime <- strptime(quizdata[j,m],"%Y-%m-%d %H:%M:%S")
                    if(!is.na(atime)){
                        if (atime$year == quizyear-1904){
                            # this means .xlsx file used 1900 format
                            # add one day because Excel thinks 1900 was a leap year
                            atime$mday <- atime$mday+1
                            atime$year <- atime$year+4
                        }
                    }
                    times[index, quizquestion] = as.character(atime)  
                }
             }   
        }       
     }
        
}


### save three tables to .xlsx files
write.xlsx(responses,file = "responses.xlsx", col.names = TRUE,row.names = FALSE,showNA = TRUE)
write.xlsx(correctness,file = "correctness.xlsx", col.names = TRUE,row.names = FALSE,showNA=TRUE)
write.xlsx(times,file = "times.xlsx", col.names = TRUE,row.names = FALSE,showNA=TRUE)
write.xlsx(students,file = "CONFIDENTIAL_students.xlsx", col.names = TRUE,row.names = FALSE,showNA=TRUE)

### save responses, correctness and times as .csv as welll
write.csv(responses,file = "responses.csv")
write.csv(correctness,file = "correctness.csv")
write.csv(times,file = "times.csv")

