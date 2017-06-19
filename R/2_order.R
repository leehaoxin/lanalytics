plot_order <- function(df_quizzes){
  df_quizzes %>% 
    ggplot(aes(x = factor(n), y = factor(question), group = id)) +
    facet_wrap(~quiz, ncol = 2) +
    geom_point(alpha = .1) + 
    geom_abline(slope=1, intercept=0, color = "red")
}

