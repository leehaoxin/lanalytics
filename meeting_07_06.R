library(devtools)

install_github("savrgg/lanalytics")

library(pkgdown)
library(tidyverse)
library(lanalytics)
library(stringr)
library(shiny)

# quiz object
file_to_read <- "datasets/Dataset1/Quiz3_session12098.csv"
file_cognitivelevel <- "datasets/Quiz2013-14_cognitive level_HB.csv" 
cognitive_level <- data.frame(read_csv(file_cognitivelevel))
quiz_object <- read_lc(file_to_read)
class(quiz_object)
head(quiz_object)

# add times per question and order time
quiz_object <- add_times(quiz_object)
head(quiz_object)

# different example plots (discuss this point)
lanalytics::plot_order(quiz_object)
lanalytics::plot_guessers(quiz_object)
lanalytics::plot_easiness_time(quiz_object)
lanalytics::plot_etl(quiz_object, cognitive_level)

# all can be gathered in a shiny:
runGitHub(repo = "savrgg/lanalytics", subdir = "shinyapp/app.R")

# or go to (limited hours):
# https://salvadorgarcia.shinyapps.io/shinyapp/

# the documentation of the package will be with pkgdown. Everything else is hosted in github:
build_site()




# points to discuss:
# 1) use of ggvis in the package 
# 2) content of the package (my idea is to have one tab at group level, 
# then click on some student and in student level tab display its own grades/analysis and plots)


