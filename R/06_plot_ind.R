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
#' quiz_object_1 <- read_lc(file_to_read)
#' file_to_read <- "../../datasets/sample_dataset/Q02.csv"
#' quiz_object_2 <- read_lc(file_to_read)
#' file_to_read <- "../../datasets/sample_dataset/Q03.csv"
#' quiz_object_3 <- read_lc(file_to_read)
#' file_to_read <- "../../datasets/sample_dataset/Q04.csv"
#' quiz_object_4 <- read_lc(file_to_read)
#' quiz_object <- bind_rows(list(quiz_object_1, quiz_object_2, quiz_object_3, quiz_object_4))
#' final_exam <- "../../datasets/sample_dataset/finalexam_file.csv"
#' final_exam <- read_csv(final_exam)
#' email_filtered <- "UTKIJ1509E@gmail.com"
#' plot_group(quiz_object, email_filtered, final_exam)
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