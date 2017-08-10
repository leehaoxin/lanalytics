#' @title Order plot with ggplot2
#' 
#' @description This function computes a graph with the order of answering of each student for each quiz.
#'
#' @param df_quizzes Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the order of answering
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' plot_order(quiz_object)
#' @export
plot_order <- function(quiz_object){
quiz_object %>% 
    dplyr::group_by(question) %>% 
    dplyr::mutate(`Tercil per time` = cut(as.numeric(`time per question`), 
                                       breaks = quantile(as.numeric(.$`time per question`), 
                                                         probs = c(0, .33, .66, 1), 
                                                         na.rm = T), 
                                       include.lowest = T, 
                                       labels = c("First Tercil (Fastest)", "Second Tercil", "Third Tercil (Slowest)")),
                  temp = sum(!is.na(`Tercil per time`)), 
                  `Tercil per time` = if_else(temp > 10, as.character(`Tercil per time`), "NA")) %>% 
    dplyr::filter(!is.na(`Tercil per time`), `Tercil per time`!= "NA") %>% 
    dplyr::group_by(question, `Tercil per time`) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = factor(question), 
                        y = `mean score` * 100, 
                        group = `Tercil per time`, 
                        color = `Tercil per time`,
                        label = round(`mean score` * 100, 0))) +
    ggplot2::facet_wrap(~`Tercil per time`, ncol = 1) +
    geom_line() +
    geom_point() +
    geom_smooth() +
    labs(x = "Question in the quiz",
         y = "Average score per tercil (max score = 100)") +
    ylim(0, 100) +
    geom_label(alpha = .7) 
}


