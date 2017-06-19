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
library(testthat)
library(tidyverse)
library(xlsx)

source("1_parse.R")
source("2_order.R")
source("3_easiness_time.R")
source("4_guessers.R")
source("5_ETL.R")

# idea to fix the code: all to character then only convert to POSIX
dir <- "data/Dataset1/"
df_test <- parse_lc(dir) # 1
plot_order(df_test) # 2
plot_easiness_time(df_test) # 3
guessers(df_test) # 3

library(shiny)
startIRT()

# https://cran.r-project.org/web/views/Psychometrics.html