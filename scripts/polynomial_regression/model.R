# Polynomial regression

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

rec_poly2 <- tab_model %>% 
  slice(1) %>% 
  recipe(o3_mass_conc ~ .) %>% 
  step_interact(terms = ~ dayofweek:dv_publicholiday) %>%
  step_mutate(
    month = as.factor(month), 
    dayofweek = as.factor(dayofweek),
    dayofweek_x_dv_publicholiday = as.factor(dayofweek_x_dv_publicholiday)
  ) %>% 
  step_poly(share_gas, degree = 2) %>% 
  step_dummy(month, starts_with("dayofweek"))

rec_poly3 <- tab_model %>% 
  slice(1) %>% 
  recipe(o3_mass_conc ~ .) %>% 
  step_interact(terms = ~ dayofweek:dv_publicholiday) %>%
  step_mutate(
    month = as.factor(month), 
    dayofweek = as.factor(dayofweek),
    dayofweek_x_dv_publicholiday = as.factor(dayofweek_x_dv_publicholiday)
  ) %>% 
  step_poly(share_gas, degree = 3) %>% 
  step_dummy(month, starts_with("dayofweek"))

# rec_poly2 %>% prep(tab_model) %>% bake(tab_model)

# Model -------------------------------------------------------------------

model <- linear_reg() %>% 
  set_engine("lm")

# Workflow ----------------------------------------------------------------

wf_poly2 <- workflow() %>% 
  add_recipe(rec_poly2) %>% 
  add_model(model)

wf_poly3 <- workflow() %>% 
  add_recipe(rec_poly3) %>% 
  add_model(model)

# cv ----------------------------------------------------------------------

# Degree 2
set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10)

fit_cv_poly2 <- fit_resamples(
  wf_poly2,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_resamples(verbose = TRUE)
)

tab_metrics <- fit_cv_poly2 %>% 
  collect_metrics()

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/poly_regression_degree2_metrics.rds"
)

# Degree 3
set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10)

fit_cv_poly3 <- fit_resamples(
  wf_poly3,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_resamples(verbose = TRUE)
)

tab_metrics <- fit_cv_poly3 %>% 
  collect_metrics()

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/poly_regression_degree3_metrics.rds"
)

# final model -------------------------------------------------------------

# Dregree 2
fit_model <- wf_poly2 %>% fit(tab_model)

tab_estimates <- fit_model %>% 
  extract_model() %>% 
  tidy()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/poly_regression_degree2_estimates.rds"
)


# Dregree 3
fit_model <- wf_poly3 %>% fit(tab_model)

tab_estimates <- fit_model %>% 
  extract_model() %>% 
  tidy()

readr::write_rds(
  tab_estimates, 
  "results/dom_pedro_ii/poly_regression_degree3_estimates.rds"
)

