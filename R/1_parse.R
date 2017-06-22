read_lc <- function(file){
  quiz_sheet <- read_csv(file) %>%  
    data.frame() %>% 
    setNames(tolower(names(.))) %>% 
    dplyr::filter(str_detect(email.address, "@")) %>% data.frame() %>% 
    dplyr::mutate(email.address = as.character(email.address),
           id = md5(email.address)) 
  
  quiz_long <- lapply(c("response", "responded.at", "score"), function(df_subset){
    # if(df_subset == "responded.at"){
    #   cols_select <- c("email.address", "id", "last.name", "first.name", 
    #                    quiz_sheet[sapply(quiz_sheet, is.POSIXt)] %>% names())
    # }else{
      cols_select <- c("email.address", "id", "last.name", "first.name", 
                       names(quiz_sheet)[str_detect(names(quiz_sheet), df_subset)])
    #}
    
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
           time_per_quiz = sum(time_per_question, na.rm = T)) %>% data.frame %>% 
    dplyr::select(-response)
}





# dir <- "test_data/Dataset1/"
# filenames <- list.files(dir, pattern="*.xlsx")
# 
# file_1 <- filenames[[1]]
# file_2 <- filenames[[2]]
# file_3 <- filenames[[3]]
# 
# quiz_info <- lapply(filenames, function(curr_file){
#   file = paste(dir, curr_file, sep = "")  
#   quiz_sheet = read.xlsx(file, 1, as.data.frame=TRUE) 
# })
# 
# bind_rows(quiz_info) %>% 
#   filter(!is.na(response)) %>% 
#   mutate(question = as.numeric(str_replace_all(question, '[A-Za-z\\.]', "")))
# 
# quiz_sheet_p <- paste(dir, file_1, sep = "")  
#   
# parse_lc <- function(quiz_sheet){
#     quiz_sheet <- quiz_sheet_p %>% 
#       setNames(tolower(names(.))) %>% 
#       filter(str_detect(email.address, "@")) %>% data.frame() %>% 
#       mutate(email.address = as.character(email.address),
#              id = md5(email.address)) 
#     
#     quiz_long <- lapply(c("response", "responded.at", "score"), function(df_subset){
#       if(df_subset == "responded.at"){
#         cols_select <- c("email.address", "id", "last.name", "first.name", 
#                          quiz_sheet[sapply(quiz_sheet, is.POSIXt)] %>% names())
#       }else{
#         cols_select <- c("email.address", "id", "last.name", "first.name", 
#                          names(quiz_sheet)[str_detect(names(quiz_sheet), df_subset)])
#       }
#       
#       quiz_long_join <- quiz_sheet[names(quiz_sheet) %in% cols_select] %>% 
#         gather(question, value, -c(email.address, id, last.name, first.name)) %>% 
#         separate(col = question, into = c("question", "round", "variable"), sep = "\\.\\.") %>% 
#         dplyr::select(-round, -variable) 
#       names(quiz_long_join)[which(names(quiz_long_join) == "value")] <- df_subset
#       quiz_long_join %>% data.frame
#     }) 
#     reduce(quiz_long, left_join) %>% 
#       mutate(quiz = curr_file,
#              response = as.character(response)) %>% 
#       group_by(quiz, id) %>%
#       arrange(quiz, id, responded.at) %>% 
#       mutate(n = 1:n(),
#              time_per_question = responded.at - lag(responded.at),
#              time_per_quiz = sum(time_per_question, na.rm = T)) %>% data.frame
# }
# 
# 
