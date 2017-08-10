#' @title histogram of the quiz with ggplot2
#' 
#' @description This function computes an histogram of the grades
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the histogram
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' quiz_object <- read_lc(file_to_read)
#' plot_hist(quiz_object)
#' @export
plot_hist <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = `total score`*10)) +
    geom_histogram(binwidth = 10, alpha = .7, 
                   color = "dodgerblue4", fill = "dodgerblue4") +
    labs(x = "Total score (max score = 100)", 
         y = "Count") +
    facet_wrap(~quiz, ncol = 2)
}