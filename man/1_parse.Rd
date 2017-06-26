\name{read_lc}
\alias{read_lc}
\title{Read file exported from Learning Catalytics}
\description{
This function receives as input a learning catalytics *csv* file and parse it to create a long format data frame.
}
\usage{
  read_lc(quiz_dir)
  This function only receives as input the directory where all the quizzes files are stored.
}
\examples{
  file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
  head(read_lc(file_to_read))
}
