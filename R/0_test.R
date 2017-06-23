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
# file <- "datasets/Dataset1/Quiz1_session1_wide format.csv"
# 
# # 1) Loading function
# df_test <- read_lc(file)
# 
# # 2) Plotting function order
# plot_order(df_test)
# 
# # 3) Plotting function easiness time
# plot_easiness_time(df_test)
# 
# # 4) Plotting function guessers
# guessers(df_test)
#
# # 5) Plotting etl
# etl(df_test)
#
# # 2) Example of documentation =========
# build_site()
# 
# 
# # 3) Example of shiny ========
# runGitHub("lanalytics", "savrgg", subdir = "shinyapp/")
#
# x = seq(0,1, .01)
# y = -x*log(x)
# y2 = -log(x)
# 
# data.frame(x = c(x, x), y = c(y, y2), modelo = c(rep("entropy", 101), rep("likelihood", 101))) %>% 
#   ggplot(aes(x = x, y = y, group = modelo, color = modelo))+
#   geom_line()
