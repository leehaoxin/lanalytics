\name{plot_easiness_time}
\alias{plot_easiness_time}
\title{Easiness-Time plot.}
\description{
This function computes a graph displaying the easiness-time plot for each quiz.
}
\usage{
  plot_easiness_time(quiz_data_frame)
  This function receives as input the dataframe un long format of all quizzes (Generated with read_lc).
}
\examples{
  file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
  long_format <- read_lc(file_to_read)
  plot_easiness_time(long_format)
}
