guessers <- function(df_quizzes){
  thresholds_df <- df_test %>% 
    group_by(quiz) %>% 
    filter(question %in% c(1,2, max(question), (max(question)-1))) %>% data.frame() %>% 
    group_by(id, quiz) %>% 
    dplyr::summarise(threshold = min(time_per_question, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) %>% data.frame() 
    
  guessing <- df_test %>% 
    left_join(thresholds_df) %>% 
    mutate(guessing = ifelse((!is.na(time_per_question) & time_per_question < threshold), 
                             ifelse(score == 1, 1, -1), 0)) %>% arrange(time_per_question) 
    
  guessing %>% mutate(quiz_response=paste(quiz, question, sep= "_")) %>% 
    dplyr::select(id, guessing, quiz_response) %>% filter(guessing != 0) %>%
    ggplot(aes(x = as.factor(id), y = quiz_response, color = factor(guessing))) +
    geom_point()  
}

