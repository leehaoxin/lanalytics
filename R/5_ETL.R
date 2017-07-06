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
#' quiz_object <- read_lc(file_to_read)
#' cognitive_level <- data.frame(read_csv(file_cognitivelevel))
#' quiz_object <- add_times(quiz_object)
#' plot_etl(quiz_object, cognitive_level)
#' @export
plot_etl <- function(quiz_object, challengeLevel){
  homo_quiz_object <- quiz_object %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>%
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, `mean time`, `mean score`)
  
  homo_challenge_level <- challengeLevel %>% 
    dplyr::rename(question_id = MCM.2014.item,
                  rating = Rating.HB) %>% 
    dplyr::select(question_id, rating)
  
  homo_quiz_object %>% 
    dplyr::left_join(homo_challenge_level) %>% 
    dplyr::mutate(`mean time` = as.numeric(`mean time`),
                  rating = factor(rating)) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score`, 
                        color = rating, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_text()
}

