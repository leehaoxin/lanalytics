library(tidyverse)
library(forcats)

read_csv("models/likert_evals.csv") %>% 
  setNames(c("tab", "evaluation", "eval1", "eval2", "eval3")) %>% 
  gather(eval, score, eval1:eval3) %>% 
  mutate(evaluation = factor(evaluation)) %>% 
  mutate(evaluation = fct_reorder(evaluation, score, desc = T)) %>% 
  ggplot(aes(x = evaluation, y = score,
             color = tab, group = tab)) +
  geom_point(show.legend = F) +
  coord_flip() +
  facet_grid(tab~., 
             scales = "free",
             space = "free") +
  labs(title = "Likert evaluations", ylab = "") +
  geom_jitter(height = 0, width = .3, show.legend = F)
  
  