# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Data --------------------------------------------------------------------

tab_model <- read_rds("data/data_model.rds")


# Number os NAs -----------------------------------------------------------

n_rows_without_NAs <- tab_model %>% 
  filter_all(~!is.na(.x)) %>% 
  nrow()

n_rows_with_NAs <- nrow(tab_model) - n_rows_without_NAs

prop_rows_with_NAs <- n_rows_with_NAs/nrow(tab_model)
prop_rows_with_NAs <- scales::percent(prop_rows_with_NAs, accuracy = 0.1)

# Observations by month ---------------------------------------------------

tab_model %>% 
  count(year, month)
