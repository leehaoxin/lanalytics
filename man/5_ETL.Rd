\name{plot_etl}
\alias{plot_etl}
\title{Easiness-time-cognitive level plot.}
\description{
This function computes a graph displaying the guessers plot for each quiz.
}
\usage{
  plot_etl(quiz_data_frame)
  This function only receives as input the directory where all the quizzes files are stored.
}
\examples{
file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
file_cognitivelevel <- "../../datasets/Quiz2013-14_cognitive level_HB.csv" 
  long_format <- read_lc(file_to_read)
  cognitive_level <- data.frame(read_csv(file_cognitivelevel))
  plot_etl(long_format, cognitive_level)
}
