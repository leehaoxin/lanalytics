library(eRm)

file_to_read <- "datasets/Dataset1/Quiz3_session12098.csv"
quiz_object <- read_lc(file_to_read)

data_tibble <- quiz_object %>% 
  ungroup() %>% 
  dplyr::select(`email address`, question, score) %>% 
  tidyr::spread(question, score, fill = 0) %>% 
  dplyr::select(-`email address`) %>% 
  stats::setNames(paste("item", names(.))) %>% 
  purrr::map_if(is.character, as.numeric) %>% 
  tibble::as_tibble() %>% 
  purrr::discard(~sum(.)<03) %>% 
  purrr::discard(~sum(.)>97) %>% as.matrix()

model <- RM(data_tibble)
# model
summary(model)
coef(model)
vcov(model)
confint(model, "beta")
logLik(model)
plotjointICC(model, xlim = c(-8, 5))
plotPImap(model)

# Person parameter estimation
pp <- person.parameter(model)
plot(pp)
confint(pp)
logLik(pp)

#lrt <- LRtest(model, se = TRUE)
#Waldtest(model)
#t11 <- NPtest(data_tibble, method = "T11")
#t11


