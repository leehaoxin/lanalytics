#' @title Read file exported from Learning Catalytics
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
#' file_to_read <- "datasets/Dataset1/Quiz3_session12098.csv"
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
