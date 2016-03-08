# question_type_analysis.R
# reads instructor-generated cognitive level file
# determines total number of quiz questions and questions in different categories
# depends on format of user-generated files; won't easily generalise 
# written by MI Stefan

## load uesful libraries
library(xlsx)       # import and export xlsx files

## set working directory
setwd("~/Work/MCM/academy_project/lanalytics")

## read input file
cogLevelData <- read.xlsx("Comparison_of_cognitive_level_2013-2014.xlsx",1)


