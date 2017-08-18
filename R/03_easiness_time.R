#' @title Easiness-Time plot.
#' 
#' @description This function computes a graph displaying the easiness-time plot for each quiz.
#'
#' @param df_quizzes receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the easiness-time of the selected quiz
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' plot_easiness_time(quiz_object)
#' @export
plot_easiness_time <- function(quiz_object){
  quiz_object %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>% 
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    ggplot2::ggplot(aes(x = as.numeric(`mean time`), 
                        y = `mean score`* 100, 
                        label = paste0("q", `question`))) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm") +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)", 
         x = "Median of taken time to answer each item (in seconds)") +
    facet_wrap(~quiz, ncol = 1)
}