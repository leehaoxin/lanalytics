library(lubridate)
# Create syntetic dataset -------------------------------------------------
digits = 0:9
createRandString<- function(x) {
  v = c(sample(LETTERS, 5, replace = TRUE),
        sample(digits, 4, replace = TRUE),
        sample(LETTERS, 1, replace = TRUE))
  return(paste0(paste0(v, collapse = ""), "@gmail.com"))
}

data.frame(`Email address` = sapply(1:100, createRandString)) %>% 
  mutate(`item01 response`= as.numeric(rbernoulli(100, .80)),
         `item02 response` = as.numeric(rbernoulli(100, .75)),
         `item03 response` = as.numeric(rbernoulli(100, .70)),
         `item04 response` = as.numeric(rbernoulli(100, .65)),
         `item05 response`= as.numeric(rbernoulli(100, .60)),
         `item06 response`= as.numeric(rbernoulli(100, .55)),
         `item07 response`= as.numeric(rbernoulli(100, .50)),
         `item08 response`= as.numeric(rbernoulli(100, .45)),
         `item09 response`= as.numeric(rbernoulli(100, .40)),
         `item10 response`= as.numeric(rbernoulli(100, .35)),
         `item11 response`= as.numeric(rbernoulli(100, .30))
         )






