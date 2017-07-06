
<!-- README.md is generated from README.Rmd. Please edit that file -->
**This project is currently under development**
===============================================

lanalytics
==========

Overview
--------

The lanalytics package provides tools to ease the analysis and visualization of online quizzes at three different levels: per quiz, per person and per course. Currently, the package can process the quizzes from csv files of two different sources **Google Forms** and **Learning Catalytics**. If the quizzes data is from any other software, is necessary to wrangle the dataset in the specified input format. In addition to the package, a [shiny dashboard](https://rstudio.github.io/shinydashboard/) was to visualize the plots with [ggviz](http://ggvis.rstudio.com/). (in progress)

Installation
------------

``` r
# The lanalytics package is based in the tidyverse, so it is necesary to install this package:
install.packages("tidyverse")

# Also, if the Shiny Dashboard wants to be used, the shiny and ggviz packages should be installed:
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggviz")

# Finally, the lanalytics package can be installed from Github (using devtools library):
install.packages("devtools")
devtools::install_github(repo = "savrgg/lanalytics")
```

Getting started with the package
--------------------------------

First of all, the tidyverse and lanalytics libraries should be loaded:

``` r
library(tidyverse)
library(lanalytics)
```

For each file, the lanalytics package creates a **quiz object** with several properties and a special format (not yet):

``` r
# modify modify
```

Also, different quiz objects can be joined together to create a **course object** (not yet):

``` r
# modify modify
```

Getting started with the shiny dashboard
----------------------------------------

To use the dashboard, the shiny and shinydashboard should be loaded:

``` r
library(ggvis)
library(shiny)
library(shinydashboard)
```

To take full advantage of the functions in the package, the Shiny Dashboard can be deploy in three ways:

``` r
# First way: Run from the shinyApps webpage:
https://salvadorgarcia.shinyapps.io/shinyapp/

# Second way: Run Shiny Dashboard from Github
shiny::runGitHub(repo = "savrgg/lanalytics", subdir = "shinyapp/app.R")

# Third way: Clone the repository and run locally
shiny::runApp("shinyapp/app.R")
```

<!-- #  -->
<!-- # At per quiz level,  -->
<!-- #  -->
<!-- # It is especially designed to process the outputs from Learning Catalytics software and from Google Forms -->
<!-- #  -->
<!-- # and functions and quizzes from three source of data. The first  -->
<!-- #  -->
<!-- # For each quiz, the package creates a quizz object -->
<!-- #  -->
<!-- # For each course, the package creates a course object -->
<!-- #  -->
<!-- # As an additional tool,  -->
<!-- #  -->
<!-- # Analysis of student data from quizzes -->
<!-- #  -->
<!-- # Files: -->
<!-- #  -->
<!-- # 1. parsequizzes.R -->
<!-- #  -->
<!-- # - First data parsing step - this script is designed to be run exactly once at  -->
<!-- #     the beginning of the data analysis process -->
<!-- # - Takes xlsx file for each quiz as downloaded from Learning Catalytics -->
<!-- # - Assigns each student a unique random identifier -->
<!-- # - Creates 3 tables, all of which have one row per student ID, and one column per    -->
<!-- #     quiz question. Data in those tables are  -->
<!-- #     - Absolute time of answer -->
<!-- #     - Content of answer -->
<!-- #     - Correctness of answer (1 or 0) -->
<!-- #     All three tables are saved into xlsx files -->
<!-- # - Creates an additional (confidential) table of student IDs and names, to be -->
<!-- #     stored in a safe place -->
<!-- #  -->
<!-- #  -->
<!-- # 2. order_time.R -->
<!-- #  -->
<!-- # - Depends on parsequizzes.R -->
<!-- # - Takes absolute time stamp data as computed using parsequizzes.R -->
<!-- # - For each quiz and each student, computes  -->
<!-- #     - the order in which questions were answered -->
<!-- #     - the time it took to answer each question -->
<!-- #  -->
<!-- #  -->
<!-- # 3. easiness_time.R -->
<!-- #  -->
<!-- # - Depends on order_time.R, parsequizzes.R  -->
<!-- # - Takes time per question and correctness information -->
<!-- # - For each quiz question, compute -->
<!-- #     - item easiness (% of sdtudents who g) -->
<!-- #     - time to answeer item (median across all students) -->
<!-- # - scatter plots easiness vs time -->
<!-- #  -->
<!-- #  -->
<!-- # 4. guessers.R -->
<!-- #  -->
<!-- # - Depends on order_time.R, parsequizzes.R -->
<!-- # - Assumes that the first two and last two questions of each quiz are easy  -->
<!-- #     (in our case, because they are about study style and preparaation) -->
<!-- # - Uses time for those easy question as a threshold (i.e. fastest possible) -->
<!-- # - Computes questions answered below this threshold and classifies them as -->
<!-- #     - cheating, if the answer was correct -->
<!-- #     - guessing, if the answer was wrong -->
<!-- # - Plots overall occurence of guessing and cheating across quizzes and students -->
<!-- #  -->
<!-- # 5. easiness_time_level.R -->
<!-- #  -->
<!-- # - Depends on easiness_time.R -->
<!-- # - Also depends on a rating of cognitive level for each question (as provided by -->
<!-- #     the instructor, 1=low, 3=high) -->
<!-- # - Plots scatter plot of easiness vs time, colour-coded by cognitive level -->
<!-- #  -->
<!-- # 6. easiness_time_level_two_years.R -->
<!-- #  -->
<!-- # - Depends on easiness_time.R -->
<!-- # - Also depends on a rating of cognitive level for each question (as provided by -->
<!-- #     the instructor, 1=low, 3=high) -->
<!-- # - Does the same as easiness_time_level.R, but for two successive years -->
<!-- # - Plots data from both years side-by-side -->
<!-- #  -->
<!-- # 7. import_time_easiness.R -->
<!-- #  -->
<!-- # - Now obsolete!  -->
<!-- # - Helper script to import data from excel for further processing -->
<!-- # - Written in order to be able to use earlier data we processed in MATLAB -->
<!-- # - Not part of the analysis workflow if everything is done in lanalytics  -->
<!-- #  -->
<!-- # 8. small change -->
