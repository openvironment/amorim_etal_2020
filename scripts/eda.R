# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

tab_model <- write_csv("data/data_model.csv")


# Number os NAs -----------------------------------------------------------

n_rows_without_NAs <- tab_model %>% 
  filter_all(~!is.na(.x)) %>% 
  nrow()

n_rows_with_NAs <- nrow(tab_model) - n_rows_without_NAs

prop_rows_with_NAs <- n_rows_with_NAs/nrow(tab_model)
prop_rows_with_NAs <- scales::percent(prop_rows_with_NAs, accuracy = 0.1)
