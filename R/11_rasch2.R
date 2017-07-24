#' @title Rasch model plot with ggplot2
#' 
#' @description This function computes a graph of the ICC of the Rasch model
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the rasch model
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' quiz_object <- read_lc(file_to_read)
#' plot_rasch2(quiz_object)
#' @export


plot_rasch2 <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  plotjointICC(model, xlim = c(-8, 5))
}  

