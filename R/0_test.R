# library(devtools)
# library(fields)
# library(Hmisc)
# library(lubridate)
# library(knitr)
# library(openssl)
# library(pkgdown)
# library(random)
# library(RColorBrewer)
# library(roxygen2)
# library(rstudioapi)
# library(stringr)
# library(shiny)
# library(shinydashboard)
# library(testthat)
# library(tidyverse)
# library(xlsx)
# 
# source("R/1_parse.R")
# source("R/2_order.R")
# source("R/3_easiness_time.R")
# source("R/4_guessers.R")
# 
# # 1) Example of functions in the package =========
# # Long format, each row is a question per quiz per person
# file <- "data/Dataset1/Quiz1_session1_wide format.xlsx"
# 
# # Loading function
# df_test <- read_lc(file)
# df_test %>% head
# 
# # Plotting function
# plot_order(df_test)
# 
# # 2) Example of documentation =========
# build_site()
# 
# 
# # 3) Example of shiny ========
# runGitHub("lanalytics", "savrgg", subdir = "shinyapp/")
