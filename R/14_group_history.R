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
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' plot_group(quiz_object)
#' @export
plot_group <- function(quiz_object){
  quiz_object %>% 
    group_by(`email address`, quiz) %>% 
    dplyr::summarise(`mean score` = round(100*mean(as.numeric(score), na.rm = T), 0)) %>% 
    ungroup() %>% 
    ggplot(aes(x = quiz, y = `mean score`, group = `email address`)) +
    geom_line() + geom_point()
}
