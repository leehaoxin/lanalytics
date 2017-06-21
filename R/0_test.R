library(devtools)
library(fields)
library(Hmisc)
library(lubridate)
library(knitr)
library(openssl)
library(random)
library(RColorBrewer)
library(roxygen2)
library(rstudioapi)
library(stringr)
library(shiny)
library(testthat)
library(tidyverse)
library(xlsx)

source("R/1_parse.R")
source("R/2_order.R")
source("R/3_easiness_time.R")
source("R/4_guessers.R")

# idea to fix the code: all to character then only convert to POSIX
file <- "data/Dataset1/Quiz1_session1_wide format.xlsx"
read_lc(file) %>% head

dir <- "data/Dataset1/"
df_test <- parse_lc(dir) # 1
plot_order(df_test) # 2
plot_easiness_time(df_test) # 3
guessers(df_test) # 3

library(shiny)
startIRT()

# https://cran.r-project.org/web/views/Psychometrics.html


runGitHub(repo = "shiny_example", 
          username = "rstudio", 
          subdir = "shinyapp/")

