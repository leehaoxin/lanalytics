library(lubridate)
latemail <- function(N, st="2017/01/01", et="2017/12/31") {
     st <- as.POSIXct(as.Date(st))
     et <- as.POSIXct(as.Date(et))
     dt <- as.numeric(difftime(et,st,unit="sec"))
     ev <- sort(runif(N, 0, dt))
     rt <- st + ev 
     rt
}



# Create syntetic dataset -------------------------------------------------
set.seed(123456)
digits = 0:9
createRandString<- function(x) {
  v = c(sample(LETTERS, 5, replace = TRUE),
        sample(digits, 4, replace = TRUE),
        sample(LETTERS, 1, replace = TRUE))
  return(paste0(paste0(v, collapse = ""), "@gmail.com"))
}

xx <- tibble(`email address` = sapply(1:100, createRandString)) %>% 
  mutate(`item01 score`= as.numeric(rbernoulli(100, .73)),
         `item02 score` = as.numeric(rbernoulli(100, .70)),
         `item03 score` = as.numeric(rbernoulli(100, .66)),
         `item04 score` = as.numeric(rbernoulli(100, .63)),
         `item05 score`= as.numeric(rbernoulli(100, .60)),
         `item06 score`= as.numeric(rbernoulli(100, .55)),
         `item07 score`= as.numeric(rbernoulli(100, .50)),
         `item08 score`= as.numeric(rbernoulli(100, .45)),
         `item09 score`= as.numeric(rbernoulli(100, .40)),
         `item10 score`= as.numeric(rbernoulli(100, .36)),
         `item11 score`= as.numeric(rbernoulli(100, .33)),
         `item01 responded at`= xx <- latemail(100, st="2017/01/01", et="2017/01/28") +
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

write_csv(xx, "datasets/sample_dataset/Q1.csv")

file <- "datasets/sample_dataset/Q1.csv"
# 
# 
# latemail(1, st="2017/02/01", et="2017/02/28")
# latemail(1, st="2017/03/01", et="2017/03/28")
# latemail(1, st="2017/04/01", et="2017/04/28")
# latemail(1, st="2017/05/01", et="2017/05/28")
# latemail(1, st="2017/06/01", et="2017/06/28")
# latemail(1, st="2017/07/01", et="2017/07/28")
# latemail(1, st="2017/08/01", et="2017/08/28")
# latemail(1, st="2017/09/01", et="2017/09/28")
# latemail(1, st="2017/10/01", et="2017/10/28")
# latemail(1, st="2017/11/01", et="2017/11/28")
# latemail(1, st="2017/12/01", et="2017/12/28")