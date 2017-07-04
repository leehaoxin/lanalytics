#' @title Read file exported from Learning Catalytics
#' 
#' @description This function receives as input a learning catalytics *csv* 
#' file and parse it to create a long format data frame.
#' 
#' @param file The name of the csv file to read
#'
#' @return it returns a data frame containing the columns:  
#'
#' @examples
#' file_to_read <- "../../datasets/Dataset1/Quiz3_session12098.csv"
#' head(read_lc(file_to_read))
#' @export
read_lc <- function(file){
  quiz_sheet <- readr::read_csv(file) %>%  
    data.frame() %>% 
    setNames(tolower(names(.))) %>% 
    dplyr::filter(stringr::str_detect(email.address, "@")) %>% 
    dplyr::mutate(email.address = as.character(email.address),
                  id = md5(email.address)) %>% 
    data.frame() 
    
  
  quiz_long <- lapply(c("response", "responded.at", "score"), function(df_subset){
      cols_select <- c("email.address", "id", "last.name", "first.name", 
                       names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_join <- quiz_sheet[names(quiz_sheet) %in% cols_select] %>% 
      tidyr::gather(question, value, 
                    -c(email.address, id, last.name, first.name)) %>% 
      tidyr::separate(col = question, 
                      into = c("question", "round", "variable"), 
                      sep = "\\.\\.") %>% 
      dplyr::select(-round, -variable) 
    
    names(quiz_long_join)[which(names(quiz_long_join) == "value")] <- df_subset
    quiz_long_join %>% data.frame
  }) 
  
  purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  responded.at = as.POSIXct(responded.at, 
                                            format = "%d/%m/%Y %H:%M"),
                  response = as.character(response)) %>% 
    dplyr::group_by(quiz, id) %>%
    dplyr::arrange(quiz, id, responded.at) %>% 
    dplyr::mutate(n = 1:n(),
                  time_per_question = responded.at - lag(responded.at),
                  time_per_quiz = sum(time_per_question, na.rm = T),
                  question = as.numeric(str_extract(question, "[0-9]+"))) %>% 
    data.frame %>% 
    dplyr::select(-response)
}