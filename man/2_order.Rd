\name{plot_order}
\alias{plot_order}
\title{Order plot with ggplot2}
\description{
This function computes a graph with the order of answering of each student for each quiz.
}
\usage{
  plot_order(quiz_data_frame)
  This function receives as input the dataframe un long format of all quizzes (Generated with read_lc).
}
\examples{
  file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
  long_format <- read_lc(file_to_read)
  plot_order(long_format)
}
