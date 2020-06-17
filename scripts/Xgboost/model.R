# XGBoost

# Utils -------------------------------------------------------------------

library(tidymodels)

source("scripts/utils/utils_plots.R")

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
    dayofweek = as.factor(dayofweek)
  ) %>% 
  step_dummy(month, dayofweek)

# rec %>% prep(tab_model) %>% bake(tab_model)

# Model -------------------------------------------------------------------

model <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_mode("regression") %>%
  set_engine("xgboost", nthread = 2)

# Workflow ----------------------------------------------------------------

wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)


# Hyperparameters ---------------------------------------------------------

hyperparams <- wf %>% 
  parameters() %>% 
  update(
    mtry = mtry(c(3, 14)),
    sample_size = sample_prop(c(0.5, 1))
  )

grid <- grid_max_entropy(hyperparams, size = 100)

# cv ----------------------------------------------------------------------

set.seed(5893524)
tab_cv <- rsample::vfold_cv(tab_model, v = 10)

fit_cv <- tune_grid(
  wf,
  resamples = tab_cv,
  metrics = yardstick::metric_set(rmse, mae, rsq),
  control = control_grid(verbose = TRUE),
  grid = grid
)

tab_metrics <- fit_cv %>%
  collect_metrics() %>% 
  semi_join(
    show_best(fit_cv, metric = "rmse", n = 1),
    by = c("mtry", "trees", "min_n")
  )

readr::write_rds(
  tab_metrics, 
  "results/dom_pedro_ii/xgboost_metrics.rds"
)

# final model -------------------------------------------------------------

tab_metrics <- readr::read_rds("results/dom_pedro_ii/xgboost_metrics.rds")

final_model <- boost_tree(
  trees = unique(tab_metrics$trees),
  mtry = unique(tab_metrics$mtry),
  min_n = unique(tab_metrics$min_n),
  tree_depth = unique(tab_metrics$tree_depth),
  learn_rate = unique(tab_metrics$learn_rate),
  loss_reduction = unique(tab_metrics$loss_reduction),
  sample_size = unique(tab_metrics$sample_size)
) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", num.threads = 2)

fit_model <- wf %>% 
  update_model(final_model) %>% 
  fit(tab_model)

# Interpretation

library(iml)

fit_model <- readr::read_rds("results/dom_pedro_ii/xgboost_fit.rds")

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
