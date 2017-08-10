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
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' plot_guessers(quiz_object)
#' @export
#' 
plot_guessers <- function(quiz_object){
  thresholds_df <- quiz_object %>% 
    dplyr::group_by(quiz) %>% 
    dplyr::filter(question %in% c(1,2, max(question), (max(question)-1))) %>% 
    dplyr::group_by(`email address`, quiz) %>% 
    dplyr::summarise(threshold = min(`time per question`, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) 
    
  guessing <- quiz_object %>% 
    dplyr::left_join(thresholds_df) %>% 
    dplyr::mutate(guessing = ifelse((!is.na(`time per question`) & `time per question` < threshold), 
                                    ifelse(score == 1, 1, -1), 0)) %>% 
    dplyr::arrange(`time per question`) 
    
  guessing %>% 
    dplyr::select(`email address`, guessing, question) %>% 
    dplyr::filter(guessing != 0) %>%
    ggplot2::ggplot(aes(x = `email address`, 
                        y = factor(question), 
                        color = factor(guessing))) +
    ggplot2::geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    labs(x = "Email address", y = "Question number (Item)", color = "Guessing score")
}

