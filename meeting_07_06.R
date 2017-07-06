
library(devtools)

install_github("savrgg/lanalytics")

library(tidyverse)
library(lanalytics)
library(stringr)

# quiz object
file_to_read <- "datasets/Dataset1/Quiz3_session12098.csv"
quiz_object <- read_lc(file_to_read)
class(quiz_object)
head(quiz_object)

# add times per question and total time
quiz_object <- add_times(quiz_object)

#