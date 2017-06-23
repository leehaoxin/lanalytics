# read in easiness and median time data
etl <- function(df_quizzes, challengeLevel){
  xx1 <- df_quizzes %>% 
    mutate(score = as.numeric(score)) %>% 
    group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% data.frame %>% 
    filter(rate_correct > 0, 
           median_time < 600) %>% 
    mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, median_time, rate_correct)
  
  xx2 <- challengeLevel %>% 
    dplyr::rename(question_id = MCM.2014.item,
                  rating = Rating.HB) %>% 
    dplyr::select(question_id, rating)
  
  xx1 %>% 
    left_join(xx2) %>% 
    mutate(median_time = as.numeric(median_time),
           rating = factor(rating)) %>% 
    ggplot(aes(x = median_time, 
               y = rate_correct, 
               color = rating, 
               label = question_id)) +
    geom_point() +
    geom_text()
}

