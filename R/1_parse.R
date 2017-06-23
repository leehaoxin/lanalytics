read_lc <- function(file){
  quiz_sheet <- read_csv(file) %>%  
    data.frame() %>% 
    setNames(tolower(names(.))) %>% 
    dplyr::filter(str_detect(email.address, "@")) %>% data.frame() %>% 
    dplyr::mutate(email.address = as.character(email.address),
           id = md5(email.address)) 
  
  quiz_long <- lapply(c("response", "responded.at", "score"), function(df_subset){
      cols_select <- c("email.address", "id", "last.name", "first.name", 
                       names(quiz_sheet)[str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_join <- quiz_sheet[names(quiz_sheet) %in% cols_select] %>% 
      gather(question, value, -c(email.address, id, last.name, first.name)) %>% 
      separate(col = question, into = c("question", "round", "variable"), sep = "\\.\\.") %>% 
      dplyr::select(-round, -variable) 
    names(quiz_long_join)[which(names(quiz_long_join) == "value")] <- df_subset
    quiz_long_join %>% data.frame
  }) 
  
  reduce(quiz_long, left_join) %>% 
    mutate(quiz = file,
           responded.at = as.POSIXct(responded.at, format = "%d/%m/%Y %H:%M"),
           response = as.character(response)) %>% 
    group_by(quiz, id) %>%
    arrange(quiz, id, responded.at) %>% 
    mutate(n = 1:n(),
           time_per_question = responded.at - lag(responded.at),
           time_per_quiz = sum(time_per_question, na.rm = T),
           question = as.numeric(str_extract(question, "[0-9]+"))) %>% data.frame %>% 
    dplyr::select(-response)
}