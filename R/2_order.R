#' @title Order plot with ggplot2
#' 
#' @description This function computes a graph with the order of answering of each student for each quiz.
#'
#' @param df_quizzes Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the order of answering
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' long_format <- read_lc(file_to_read)
#' plot_order(long_format)
#' @export
plot_order <- function(df_quizzes){
  df_quizzes %>% 
    ggplot2::ggplot(aes(x = factor(n), 
                        y = factor(question), 
                        group = id)) +
    ggplot2::facet_wrap(~quiz, ncol = 2) +
    ggplot2::geom_point(alpha = .1) + 
    ggplot2::geom_abline(slope=1, intercept=0, color = "red")
}

