
<!-- README.md is generated from README.Rmd. Please edit that file -->
The documentation for the project is in: <https://savrgg.github.io/lanalytics/>
===============================================================================

lanalytics
==========

Overview
--------

The lanalytics package provides tools to ease the analysis and visualization of online quizzes at three different levels: per quiz, per person and per course. Currently, the package can process the quizzes from csv files in the specified format. If the quizzes data is from any other software, is necessary to wrangle the dataset in the specified input format. In addition to the package, a [shiny dashboard](https://rstudio.github.io/shinydashboard/) was to visualize the plots with [ggviz](http://ggvis.rstudio.com/). (in future developtment)

Installation
------------

``` r
# The lanalytics package is based in the tidyverse, so it is necesary to install this package:
install.packages("tidyverse")

# Also, if you want to use the Shiny Dashboard, the following packages should be installed:
install.packages("shiny")
install.packages("shinydashboard")
install.packages("eRm")
install.packages("ggrepel")
install.packages("devtools")
install.packages("stringr")
install.packages("devtools")
install.packages("data.table")

# and loaded

library(shiny)
library(shinydashboard)
library(eRm)
library(ggrepel)
library(devtools)
library(stringr)
library(devtools)
library(data.table)
```

Getting started with the package
--------------------------------

To use the dashboard, the following command should be executed:

``` r
shiny::runGitHub(repo = "savrgg/lanalytics", subdir = "shinyapp/app.R")
```
also, the sample datasets can be used to test the package:


```r
https://github.com/savrgg/lanalytics/tree/master/datasets/sample_dataset
```

