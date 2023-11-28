library(tidymodels)
library(tidyverse)

lasso_reg <- linear_reg(penalty = tune(), mixture = 1) %>%  
  set_engine("glmnet")