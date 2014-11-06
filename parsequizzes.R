## parsequizzes.R
## parses data from all quizzes in a given directory into three separate tables
## Written by MI Stefan

library(stringr)
library(xlsx)
library(digest)
library(random)

# get input directory 
quizdir = ("/home/melanie/Dropbox/Academy Project/Raw Data 2014/")

# year quiz was administered (important for date format conversions later on)
quizyear = 2014

# make 3 master tables: responses, item correctness (1 or 0), time
responses <- data.frame()
correctness <- data.frame()
times <- data.frame()

# make table to hold student names and IDs (careful about who gets this)
students <- data.frame()

# create random sequence which will help anonymize student ids later
randseq=randomSequence(1,8)


# read in all files from input directory
filenames <- list.files(quizdir, pattern="*.xlsx")

# create random 4-number ids for students; 
# need to know number of students for that
studentNumber = 170;
studentIDs <- sample(1000:9999, studentNumber, replace = F)


# go through every quiz (need to know number of lowest and highest quiz)
for (i in 4:32){
    
    # read in xlsx file
    fileindex <- grep(paste("2014Q",i,"_",sep=""),filenames[])
    thisquiz <- filenames[fileindex]
    file = paste(quizdir,thisquiz,sep="")
    quizdata = read.xlsx(file,1, as.data.frame=TRUE)
            
    # go through every student
    for (j in 1:nrow(quizdata)){
        # get student e-mail and encrypt to 8-bit string (sufficient for us)
        mail = as.character(quizdata[j,1])
        crc32 = digest(mail,algo='crc32')
        id=""
        ## shuffle encrypted string to get id that is harder to decode
        for (k in randseq){
            id = paste(id,substr(crc32,k,k),sep="")
        }

        
        # look up student identifier and see if it's alreaday in tables
        if (length(responses) == 0){
            
            responses[1,"id"] <- id            
            correctness[1,"id"] <- id
            times[1,"id"] <- id
            students[1,"id"] <- id
            students[1,"mail"] <- mail
            index = 1
        }
        else if (length(which(responses[,1]==id) > 0)){
             index = which(responses[,1]==id)            
        } 
        else {
             # if not, create new entry
             index = nrow(responses)+1 
             responses[index,1] <- id
             correctness[index,1] <- id
             times[index,1] <- id     
             students[index,"id"] <- id
             students[index,"mail"] <- mail
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



