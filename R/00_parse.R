#' @title Read file exported from Learning Catalytics or with the specified format.
#' 
#' @description This function receives as input a learning catalytics *csv* 
#' file and parse it to create quiz object.
#' 
#' @param file The name of the csv file to read
#'
#' @return it returns a quiz object containing the following columns:  
#' "email address", "question", "responded at", "score" and "quiz"
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' quiz_object <- read_lc(file_to_read)
#' @export
read_lc <- function(file){
  quiz_sheet <- readr::read_csv(file) %>% 
    setNames(tolower(names(.)))
  
  if(!("email address" %in% names(quiz_sheet))){
    stop("Error: The file does not contain a column called \"email address\"")
  }
  
  quiz_sheet <-  quiz_sheet %>% 
    dplyr::filter(stringr::str_detect(`email address`, "@")) %>% 
    dplyr::mutate(`email address` = as.character(`email address`))
  
  quiz_long <- lapply(c("responded at", "score"), function(df_subset){
    selected_cols <- c("email address",
                       names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_sheet <- quiz_sheet[names(quiz_sheet) %in% selected_cols] %>% 
      tidyr::gather(question, value, 
                    -c(`email address`)) %>% 
      dplyr::mutate(question = str_extract(tolower(question), "item[0-9]*"),
                    question = str_replace(question, "item", "")) %>% 
      filter(!is.na(value))
    
    names(quiz_long_sheet)[which(names(quiz_long_sheet) == "value")] <- df_subset
    quiz_long_sheet
  }) 
  
  quiz_long <- purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  `responded at` = parse_datetime(x = `responded at`))
  class(quiz_long) <- c("quizz", class(quiz_long))
  quiz_long %>% 
    mutate(score = as.integer(score))
}