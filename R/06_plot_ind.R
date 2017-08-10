#' @title Rasch model plot with ggplot2
#' 
#' @description This function computes a graph of the ICC of the Rasch model
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the rasch model
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' final_exam <- "../../datasets/sample_dataset/finalexam_file.csv"
#' quiz_object <- read_lc(file_to_read)
#' final_exam <- data.frame(read_csv(final_exam))
#' email_filtered <- UTKIJ1509E@gmail.com
#' plot_group(quiz_object, email_filtered, df_exam)
#' @export
plot_group <- function(quiz_object, email_filtered, df_exam = NULL){
  if(is.null(df_exam)){
    df_exam = tibble(`email address` = quiz_object$`email address` %>% unique, 
                     `final exam` = rep(NA, 100))
  }
  quiz_object %>% 
    left_join(df_exam) %>% 
    group_by(`email address`, quiz, `final exam`) %>% 
    dplyr::summarise(`mean score` = round(100*mean(as.numeric(score), na.rm = T), 0)) %>% 
    filter(`email address` %in% email_filtered) %>%
    ggplot(aes(x = quiz, 
               y = `mean score`, 
               group = `email address`)) +
    geom_line() + 
    geom_point() +
    geom_hline(aes(yintercept = `final exam`), color = "red") + 
    facet_wrap(~`email address`, ncol = 2)
}