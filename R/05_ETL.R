#' @title Easiness-time-level plot.
#'
#' @description This function computes a graph displaying the guessers plot for each quiz.
#' 
#' @param df_quizzes dataframe in long format of all quizzes (Generated with read_lc)
#' @param challengeLevel file of challengue level
#'
#' @return The ETL plot
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' file_cognitivelevel <- "../../datasets/sample_dataset/cognitive_file.csv" 
#' quiz_object <- read_lc(file_to_read)
#' cognitive_level <- data.frame(read_csv(file_cognitivelevel))
#' quiz_object <- add_times(quiz_object)
#' plot_etl(quiz_object, cognitive_level)
#' @export
plot_etl <- function(quiz_object, challengeLevel, item = "MCM.2014.item", rating = "Rating.HB"){
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
    dplyr::select(item, rating) %>% 
    setNames(c("question_id", "rating"))
  
  rating_df <- data.frame(rating = factor(c(1,2,3)), 
                          rating2 = factor(c("Low cognitive level", 
                                             "Medium cognitive level", 
                                             "High cognitive level")))
  
  homo_quiz_object %>% 
    dplyr::left_join(homo_challenge_level) %>% 
    dplyr::mutate(`mean time` = as.numeric(`mean time`),
                  rating = factor(rating)) %>% 
    left_join(rating_df) %>% 
    dplyr::select(-rating) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score` *100, 
                        color = rating2, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)",
         x = "Average time taken to answer each item (in seconds)",
         color = "Cognitive level") +
    facet_wrap(~quiz, ncol = 1)
}