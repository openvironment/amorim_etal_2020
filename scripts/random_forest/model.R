# Random forest

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
  )

# rec %>% prep(tab_model) %>% bake(tab_model) %>% View()

# Model -------------------------------------------------------------------

model <- rand_forest(
  trees = 1500,
  mtry = 5,
  min_n = 4
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", num.threads = 8)

# Workflow ----------------------------------------------------------------

wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(model)


# Hyperparameters ---------------------------------------------------------

hyperparams <- wf %>% 
  parameters() %>% 
  update(
    mtry = mtry(c(3, 14))
  )

grid <- grid_max_entropy(hyperparams, size = 80)

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
  "results/dom_pedro_ii/random_forest_metrics.rds"
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
  "results/dom_pedro_ii/random_forest_metrics.rds"
)

# final model -------------------------------------------------------------

fit_model <- wf %>% 
  fit(tab_model)

# Interpretation

library(iml)
# library(ranger)

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
  "results/dom_pedro_ii/ale_plot_random_forest.png",
  width = 16,
  height = 10
)
