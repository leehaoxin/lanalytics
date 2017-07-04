#' @title Guessers per quiz plot
#'
#' @description This function computes a graph displaying the guessers plot for each quiz.
#' 
#' @param df_quizzes receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the guessers of the selected quiz
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' long_format <- read_lc(file_to_read)
#' plot_guessers(long_format)
#' @export
plot_guessers <- function(df_quizzes){
  thresholds_df <- df_quizzes %>% 
    dplyr::group_by(quiz) %>% 
    dplyr::filter(question %in% c(1,2, max(question), (max(question)-1))) %>% 
    data.frame() %>% 
    dplyr::group_by(id, quiz) %>% 
    dplyr::summarise(threshold = min(time_per_question, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) %>% data.frame() 
    
  guessing <- df_quizzes %>% 
    dplyr::left_join(thresholds_df) %>% 
    dplyr::mutate(guessing = ifelse((!is.na(time_per_question) & time_per_question < threshold), 
                                    ifelse(score == 1, 1, -1), 0)) %>% arrange(time_per_question) 
    
  guessing %>% 
    dplyr::mutate(quiz_response=question) %>% 
    dplyr::select(id, guessing, quiz_response) %>% 
    dplyr::filter(guessing != 0) %>%
    ggplot2::ggplot(aes(x = as.factor(id), 
                        y = quiz_response, 
                        color = factor(guessing))) +
    ggplot2::geom_point()  
}

