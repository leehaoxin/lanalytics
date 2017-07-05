#' @title Order plot with ggplot2
#' 
#' @description This function computes a graph with the order of answering of each student for each quiz.
#'
#' @param df_quizzes Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the order of answering
#'
#' @examples
#' file_to_read <- "datasets/Dataset1/Quiz3_session12098.csv"
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' plot_order(quiz_object)
#' @export
plot_order <- function(quiz_object){
  quiz_object %>% 
    dplyr::group_by(question) %>% 
    dplyr::mutate(`time tercil` = cut(as.numeric(`time per question`), 
                                       breaks = quantile(as.numeric(.$`time per question`), 
                                                         probs = c(0, .33, .66, 1), 
                                                         na.rm = T), 
                                       include.lowest = T, 
                                       labels = c("First Tercil", "Second Tercil", "Third Tercil"))) %>% 
    dplyr::filter(!is.na(`time tercil`)) %>% 
    dplyr::group_by(question, `time tercil`) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = question, 
                        y = `mean score`, 
                        group = `time tercil`, 
                        color = `time tercil`)) +
    ggplot2::facet_wrap(~`time tercil`, ncol = 1) +
    geom_line() +
    geom_point()
}

