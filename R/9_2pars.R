#' @title 2-pars model plot with ggplot2
#' 
#' @description This function computes a graph of the ICC of two pars model
#'
#' @param quiz_object Receives as input the dataframe un long format of all quizzes (Generated with read_lc)
#'
#' @return A plot of the rasch model
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' quiz_object <- read_lc(file_to_read)
#' plot_2pars(quiz_object, type = "ICC")
#' @export

plot_2pars <- function(quiz_object, type = c("ICC", "IIC")){
  x_vals = seq(-3.8, 3.8, length = 100)
  d_matrix <- cbind(1, x_vals)
  
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)==0) %>% dplyr::select(-`item 1`)
    
  
  model <- ltm(data_tibble ~ z1)
  betas <- model$coefficients
  
  plot_vals <- if (type == "ICC") {
    y_lab <- "Probability of correctness"
    plogis(d_matrix %*% t(betas))
  }
  else {
    y_lab <- "Information"
    temp <- plogis(d_matrix %*% t(betas))
    temp2 <- temp * (1 - temp)
    t(t(temp2) * betas[, 2]^2)
  }
  
  plot_vals %>%
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggplot(aes(x = x_vals, y = value, group = item, color = item)) +
    geom_line() +
    labs(x = "Ability", y = y_lab) 
}  

