\name{plot_guessers}
\alias{plot_guessers}
\title{Guessers per quiz plot}
\description{
This function computes a graph displaying the guessers plot for each quiz.
}
\usage{
  plot_guessers(quiz_data_frame)
  This function only receives as input the directory where all the quizzes files are stored.
}
\examples{
  file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
  long_format <- read_lc(file_to_read)
  plot_guessers(long_format)
}
