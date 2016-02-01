## total scores.R
## using data from two successive offerings of the course
## computes scores for all quizzes, per student, per quiz, and overall
## Written by MI Stefan

## load uesful libraries
library(Hmisc)
library(xlsx)       # import and export xlsx files
library(stringr)    # manipulate strings
library(ggplot2)    # extra plotting capabilities
library(RColorBrewer) # allows us to use Cynthia Brewer's color schemes

# specify output directory
output_dir="total_scores/"

