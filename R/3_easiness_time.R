#' @title Easiness-Time plot.
#' 
#' @description This function computes a graph displaying the easiness-time plot for each quiz.
#'
#' @param df_quizzes receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the easiness-time of the selected quiz
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' long_format <- read_lc(file_to_read)
#' plot_easiness_time(long_format)
#' @export
plot_easiness_time <- function(df_quizzes){
  df_quizzes %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% 
    data.frame %>%
    dplyr::filter(rate_correct > 0, 
                  median_time < 600) %>% 
    ggplot2::ggplot(aes(x = median_time, 
                        y = rate_correct)) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm")
}
