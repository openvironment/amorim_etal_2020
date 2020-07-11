# XGBoost

# Utils -------------------------------------------------------------------

library(tidymodels)

source("scripts/utils/utils_modelling.R")

theme_set(theme_minimal())

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
  step_dummy(month, dayofweek)

# rec %>% prep(tab_model) %>% bake(tab_model)

# Model -------------------------------------------------------------------

model <- boost_tree(
  mtry = 15,
  trees = 2000,
  min_n = 16,
  tree_depth = 4,
  learn_rate = 0.01,
  loss_reduction = 0.01,
  sample_size = 0.95
) %>%
  set_mode("regression") %>%
  set_engine("xgboost", nthread = 2)

# Workflow ----------------------------------------------------------------

wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)


# Hyperparameters ---------------------------------------------------------

hyperparams <- wf %>% 
  parameters() 

grid <- grid_max_entropy(hyperparams, size = 50)
# grid <- expand.grid(mtry = c(5,  7, 10, 15))

# cv ----------------------------------------------------------------------

# Tunning 
set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10, repeats = 5)

fit_cv <- tune_grid(
  wf,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_grid(verbose = TRUE),
  grid = grid
)

fit_cv %>%
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean)

tab_metrics <- fit_cv %>%
  collect_metrics() 

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/xgboost_metrics.rds"
)

# Metrics 
set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10, repeats = 5)

fit_cv <- tune::fit_resamples(
  wf,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_grid(verbose = TRUE)
)

fit_cv %>%
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean)

tab_metrics <- fit_cv %>%
  collect_metrics() 

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/xgboost_metrics.rds"
)


# final model -------------------------------------------------------------

fit_model <- wf %>% 
  fit(tab_model)

# Interpretation

library(iml)

predict_function <- function(model, newdata) {
  predict(model, new_data = newdata)$.pred
}

ale_plot(
  data = tab_model, 
  model = fit_model, 
  feature = "share_gas", 
  predict_function = predict_function, 
  grid.size = seq(10, 40, 5),
  xlab = "share25"
)

ggsave(
  "results/dom_pedro_ii/ale_plot_xgboost.png",
  width = 16,
  height = 10
)
