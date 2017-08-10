#' @title boxplot of the quiz with ggplot2
#' 
#' @description This function computes an boxplot of the grades per quiz
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the boxplot
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' quiz_object <- read_lc(file_to_read)
#' plot_boxplots(quiz_object)
#' @export
plot_boxplots <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = factor(`quiz`), 
               y = `total score`*10)) +
    geom_boxplot() +
    labs(x = "Quiz name", 
         y = "Total score (max score = 100)")
}