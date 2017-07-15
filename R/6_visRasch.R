#' @title Rasch model plot with ggvis
#' 
#' @description This function computes a dynamic graph of the ICC of the Rasch model
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the rasch model in ggvis
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' quiz_object <- read_lc(file_to_read)
#' vis_rasch(quiz_object)
#' @export
vis_rasch <- function(quiz_object){
  x_vals = seq(zrange[1], zrange[2], length = 100)
  d_matrix <- cbind(1, x_vals)
  
  data_tibble <- quiz_object %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-c(`email address`)) %>% 
    setNames(paste("item", names(.))) %>% 
    map_if(is.character, as.numeric) %>% 
    as_tibble() %>% 
    discard(~sum(.)==0)
  
  model <- rasch(data_tibble)
  betas <- model$coefficients
  
  plogis(d_matrix %*% t(betas)) %>%
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggvis(~x_vals, ~value, stroke = ~item, strokeWidth:= 3) %>% 
    layer_lines() %>% 
    group_by(item) %>% 
    add_tooltip(function(df) df$item)
}  