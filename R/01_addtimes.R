#' @title Modify the quiz_object to add the order of answering
#' and the spent time per question
#' 
#' @description This function receives as input a quiz object 
#' created with the read_lc() function
#' 
#' @param file a quiz object from read_lc
#'
#' @return it returns a quiz object containing the following columns:  
#' "email address", "question", "responded at", "score" and "quiz"
#'
#' @examples
#' file_to_read <- "../../datasets/sample_dataset/Q01.csv"
#' quiz_object <- read_lc(file_to_read)
#' quiz_object <- add_times(quiz_object)
#' @export
add_times <- function(course){
  course <- course %>% 
    dplyr::group_by(quiz, `email address`) %>%
    dplyr::arrange(quiz, `email address`, `responded at`) %>% 
    dplyr::mutate(`order answer` = 1:n(),
                  `time per question` = `responded at` - lag(`responded at`),
                  question = as.numeric(question))
}