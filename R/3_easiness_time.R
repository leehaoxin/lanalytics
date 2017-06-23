plot_easiness_time <- function(df_quizzes){
  df_quizzes %>%
    mutate(score = as.numeric(score)) %>% 
    group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% data.frame %>%
    filter(rate_correct > 0, 
           median_time < 600) %>% 
    ggplot(aes(x = median_time, y = rate_correct)) +
    geom_point()+
    geom_smooth(method = "lm")
}
