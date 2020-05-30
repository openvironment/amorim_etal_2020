# Linear regression

# Libraries ---------------------------------------------------------------

library(tidymodels)

# Data --------------------------------------------------------------------

tab_model <- readr::read_rds("data/data_model.rds")

tab_model <- tab_model %>% 
  filter(
    stationname == "Dom Pedro II"
  ) %>% 
  select(
    -siteid, -stationname,
    -date, -year, -week, -day, -dv_weekday_regular, -dv_yearendvacation,
    -dv_beltway_open
  ) %>% 
  na.omit()

# Receita -----------------------------------------------------------------

rec <- tab_model %>% 
  slice(1) %>% 
  recipe(o3_mass_conc ~ .) %>% 
  step_interact(terms = ~ dayofweek:dv_publicholiday) %>%
  step_mutate(
    month = as.factor(month), 
    dayofweek = as.factor(dayofweek),
    dayofweek_x_dv_publicholiday = as.factor(dayofweek_x_dv_publicholiday)
  ) %>% 
  step_dummy(month, starts_with("dayofweek"))

# rec %>% prep(tab_model) %>% bake(tab_model)

# Model -------------------------------------------------------------------

model <- linear_reg() %>% 
  set_engine("lm")

# Workflow ----------------------------------------------------------------

wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)


# cv ----------------------------------------------------------------------

set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10)

fit_cv <- fit_resamples(
  wf,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_resamples(verbose = TRUE)
)

tab_metrics <- fit_cv %>% 
  collect_metrics()

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/linear_regression_metrics.rds"
)

# final model -------------------------------------------------------------

fit_model <- wf %>% fit(tab_model)

tab_estimates <- fit_model %>% 
  extract_model() %>% 
  tidy()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/linear_regression_estimates.rds"
)

