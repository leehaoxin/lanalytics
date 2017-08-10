library(lubridate)
library(tidyverse)
latemail <- function(N, st="2017/01/01", et="2017/12/31") {
     st <- as.POSIXct(as.Date(st))
     et <- as.POSIXct(as.Date(et))
     dt <- as.numeric(difftime(et,st,unit="sec"))
     ev <- sort(runif(N, 0, dt))
     rt <- st + ev 
     rt
}

# Syntetic dataset quiz -------------------------------

dates <- list(latemail(100, st="2017/02/01", et="2017/02/28"),
     latemail(100, st="2017/03/01", et="2017/03/28"),
     latemail(100, st="2017/04/01", et="2017/04/28"),
     latemail(100, st="2017/05/01", et="2017/05/28"),
     latemail(100, st="2017/06/01", et="2017/06/28"),
     latemail(100, st="2017/07/01", et="2017/07/28"),
     latemail(100, st="2017/08/01", et="2017/08/28"),
     latemail(100, st="2017/09/01", et="2017/09/28"),
     latemail(100, st="2017/10/01", et="2017/10/28"),
     latemail(100, st="2017/11/01", et="2017/11/28"),
     latemail(100, st="2017/12/01", et="2017/12/28"),
     latemail(100, st="2017/01/01", et="2017/01/28"))

files <- unlist(list(paste0("Q0", 1:9), paste0("Q", 10:12)))
set.seed(123456)
digits = 0:9
createRandString<- function(x) {
  v = c(sample(LETTERS, 5, replace = TRUE),
        sample(digits, 4, replace = TRUE),
        sample(LETTERS, 1, replace = TRUE))
  return(paste0(paste0(v, collapse = ""), "@gmail.com"))
}  

rand_names <- sapply(1:100, createRandString)


lapply(1:12, function(x){
  xx <- tibble(`email address` = rand_names) %>% 
    mutate(`item01 score`= c(as.numeric(rbernoulli(33, .63)),
                             as.numeric(rbernoulli(33, .73)),
                             as.numeric(rbernoulli(34, .83))),
           `item02 score` = c(as.numeric(rbernoulli(33, .65)),
                              as.numeric(rbernoulli(33, .75)),
                              as.numeric(rbernoulli(34, .85))),
           `item03 score` = c(as.numeric(rbernoulli(33, .70)),
                              as.numeric(rbernoulli(33, .80)),
                              as.numeric(rbernoulli(34, .90))),
           `item04 score` = c(as.numeric(rbernoulli(33, .75)),
                              as.numeric(rbernoulli(33, .85)),
                              as.numeric(rbernoulli(34, .95))),
           `item05 score`= c(as.numeric(rbernoulli(33, .80)),
                             as.numeric(rbernoulli(33, .90)),
                             as.numeric(rbernoulli(34, .95))),
           `item06 score`= c(as.numeric(rbernoulli(33, .85)),
                             as.numeric(rbernoulli(33, .90)),
                             as.numeric(rbernoulli(34, .95))),
           `item07 score`= c(as.numeric(rbernoulli(33, .60)),
                             as.numeric(rbernoulli(33, .65)),
                             as.numeric(rbernoulli(34, .70))),
           `item08 score`= c(as.numeric(rbernoulli(33, .75)),
                             as.numeric(rbernoulli(33, .85)),
                             as.numeric(rbernoulli(34, .95))),
           `item09 score`= c(as.numeric(rbernoulli(33, .60)),
                             as.numeric(rbernoulli(33, .70)),
                             as.numeric(rbernoulli(34, .80))),
           `item10 score`= c(as.numeric(rbernoulli(33, .85)),
                             as.numeric(rbernoulli(33, .85)),
                             as.numeric(rbernoulli(34, .85))),
           `item11 score`= c(as.numeric(rbernoulli(33, .50)),
                             as.numeric(rbernoulli(33, .60)),
                             as.numeric(rbernoulli(34, .70))),
           `item01 responded at`= xx <- dates[[x]] +
             as.numeric(rnorm(100, mean = 20, sd = 4)),
           `item02 responded at` = `item01 responded at` + 
             as.numeric(rnorm(100, mean = 10, sd = 4)),
           `item03 responded at` = `item02 responded at` + 
             as.numeric(rnorm(100, mean = 15, sd = 4)),
           `item04 responded at` = `item03 responded at` + 
             as.numeric(rnorm(100, mean = 20, sd = 4)),
           `item05 responded at` = `item04 responded at` + 
             as.numeric(rnorm(100, mean = 20, sd = 4)),
           `item06 responded at` = `item05 responded at` + 
             as.numeric(rnorm(100, mean = 45, sd = 4)),
           `item07 responded at` = `item06 responded at` + 
             as.numeric(rnorm(100, mean = 50, sd = 4)),
           `item08 responded at` = `item07 responded at` + 
             as.numeric(rnorm(100, mean = 50, sd = 4)),
           `item09 responded at` = `item08 responded at` + 
             as.numeric(rnorm(100, mean = 50, sd = 4)),
           `item10 responded at` = `item09 responded at` + 
             as.numeric(rnorm(100, mean = 50, sd = 4)),
           `item11 responded at` = `item10 responded at` + 
             as.numeric(rnorm(100, mean = 50, sd = 4))
    )
  write_csv(xx, paste0("datasets/sample_dataset/", files[[x]],".csv"))
})

# Syntetic dataset cognitive --------------------------


# 1: 10, 5, 6
# 2: 2, 3, 4, 8
# 3: 7, 9, 11, 1
xx <- data.frame(item = paste0(rep(paste0("Q", 1:11, "_"), each = 11), rep(paste0("q", 1:11), 11)), 
           cognitive_level = c(3,2,2,2,1,1,3,2,3,1,3))

write_csv(xx, paste0("datasets/sample_dataset/cognitive_file.csv"))


# final exam dataset ----------------------------------

write_csv(xx, paste0("datasets/sample_dataset/cognitive_file.csv"))

list_files <- list.files("datasets/sample_dataset/")[-1]
xx <- lapply(list_files, function(x){
  read_csv(paste0("datasets/sample_dataset/",x))
  }
  )

xx <- bind_rows(xx) %>% 
  dplyr::select(contains("score"), `email address`) %>% 
  gather(variable, value, -`email address`) %>% 
  group_by(`email address`) %>% 
  dplyr::summarise(`final exam` = mean(value)*100+3)

write_csv(xx, paste0("datasets/sample_dataset/finalexam_file.csv"))

