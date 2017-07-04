#' @title Easiness-time-cognitive level plot.
#'
#' @description This function computes a graph displaying the guessers plot for each quiz.
#' 
#' @param df_quizzes dataframe in long format of all quizzes (Generated with read_lc)
#' @param challengeLevel file of challengue level
#'
#' @return The ETL plot
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' file_cognitivelevel <- "../../datasets/Quiz2013-14_cognitive level_HB.csv" 
#' long_format <- read_lc(file_to_read)
#' cognitive_level <- data.frame(read_csv(file_cognitivelevel))
#' plot_etl(long_format, cognitive_level)
#' @export
plot_etl <- function(df_quizzes, challengeLevel){
  xx1 <- df_quizzes %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% data.frame %>% 
    dplyr::filter(rate_correct > 0, 
                  median_time < 600) %>% 
    dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, median_time, rate_correct)
  
  xx2 <- challengeLevel %>% 
    dplyr::rename(question_id = MCM.2014.item,
                  rating = Rating.HB) %>% 
    dplyr::select(question_id, rating)
  
  xx1 %>% 
    dplyr::left_join(xx2) %>% 
    dplyr::mutate(median_time = as.numeric(median_time),
                  rating = factor(rating)) %>% 
    ggplot2::ggplot(aes(x = median_time, 
                        y = rate_correct, 
                        color = rating, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_text()
}

