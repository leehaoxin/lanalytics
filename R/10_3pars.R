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
#' plot_3pars(quiz_object, type = "ICC)
#' @export

plot_3pars <- function(quiz_object, type = c("ICC", "IIC")){
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
    purrr::discard(~sum(.)<15) %>% 
    purrr::discard(~sum(.)>85)
  
  model <- tpm(data_tibble, start.val = "random")
  thetas <- model$coefficients
  temp <- plogis(thetas[, 1]) * model$max.guessing
  betas <- thetas[, 2:3]
  p <- nrow(betas)
  
  
  plot_vals <- if (type == "ICC") {
    temp <- matrix(temp, length(x_vals), p, TRUE)
    y_lab <- "Probability of correctness"
    temp + (1 - temp) * ltm:::probs(d_matrix %*% t(betas)) 
    
  } else {
    y_lab <- "Information"
    pi_2 <- plogis(d_matrix %*% t(betas))
    temp <- matrix(temp, length(x_vals), p, TRUE)
    pi <- temp + (1 - temp) * pi_2
    pqr <- pi * (1 - pi) * (pi_2/pi)^2
    t(t(pqr) * betas[, 2]^2)
  }
  
  plot_vals %>% 
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggplot(aes(x = x_vals, y = value, group = item, color = item)) +
    geom_line() +
    labs(x = "Ability", y = y_lab) 
}  

