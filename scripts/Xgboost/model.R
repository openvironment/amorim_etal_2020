# Xgboost

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)
library(patchwork)
library(iml)

# Dados -------------------------------------------------------------------

tab_model <- read_rds("data/data_model.rds")

# Utils -------------------------------------------------------------------

pred_obs_plot <- function(obs, pred) {
  
  tibble(x = obs, y = pred) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "royal blue") +
    labs(x = "Observed", y = "Predicted") +
    theme_bw()
  
}

# Data prep ---------------------------------------------------------------

formula <- tab_model %>%
  select(
    -siteid,
    -date, -o3_mass_conc, -dayofweek,
    -starts_with("congestion"),
    -dv_kmregion_am_18_max, -dv_kmcity_am_80_max,
    -pp, -dv_pp_20_150,
    -dv_sun_reg,
    -year, -month, -day, -dv_weekday_regular, -dv_yearendvacation
  ) %>%
  names() %>%
  str_c(collapse = " + ") %>%
  str_c("o3_mass_conc ~ ", .) %>%
  as.formula()

rec <- tab_model %>%
  na.omit() %>% 
  recipe(formula = formula) %>%
  step_naomit(all_predictors(), all_outcomes()) %>% 
  step_dummy(stationname, week, one_hot = TRUE) 

# Model -------------------------------------------------------------------

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 300,
  max_depth = 5,
  eta = 0.4,
  colsample_bytree = 0.7,
  subsample = 1
)

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:5, ~sample.int(1000, nrow(distinct(tuning_grid))))
seeds[[6]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "cv", 
  number = 5,  
  verboseIter = TRUE,
  seeds = seeds
)

model <- train(
  x = rec,
  data = na.omit(tab_model),
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

# model
# varImp(model)

# Observed vs predicted plot
p <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
)

# Interpretation ------------------------------------------------------------

df_train <- rec %>% 
  prep(tab_model) %>% 
  bake(tab_model)

X <- df_train %>% 
  select(-o3_mass_conc) %>% 
  as.matrix()

tuning_grid <- expand.grid(
  gamma = 0,
  min_child_weight = 1,
  nrounds = 300,
  max_depth = 5,
  eta = 0.4,
  colsample_bytree = 0.7,
  subsample = 1
)

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:5, ~sample.int(1000, nrow(distinct(tuning_grid))))
seeds[[6]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "cv", 
  number = 5,  
  verboseIter = TRUE,
  seeds = seeds
)

model <- train(
  x = X,
  y = df_train$o3_mass_conc,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tuning_grid
)

predictor <- Predictor$new(model, data = df_train, y = df_train$o3_mass_conc)

# ALE shareE25
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 10)

p_ale_share <- ale$plot() + 
  theme_bw() +
  labs(x = "shareE25") +
  scale_y_continuous(name = "ALE") +
  ggtitle("XGBoost")

# ALE temperature

ale <- FeatureEffect$new(predictor, feature = "tp", grid.size = 10)
p_tp <- ale$plot() + 
  theme_bw() +
  labs(x = "Temperature") +
  scale_y_continuous(name = "ALE")

# ALE humidity

ale <- FeatureEffect$new(predictor, feature = "hm", grid.size = 10)
p_hm <- ale$plot() + 
  theme_bw() +
  labs(x = "Humidity") +
  scale_y_continuous(name = "ALE")

# ALE Radiation

ale <- FeatureEffect$new(predictor, feature = "rd", grid.size = 10)
p_rd <- ale$plot() + 
  theme_bw() +
  labs(x = "Radiation") +
  scale_y_continuous(name = "ALE")

# ALE wind speed

ale <- FeatureEffect$new(predictor, feature = "ws", grid.size = 10)
p_ws <- ale$plot() + 
  theme_bw() +
  labs(x = "Wind speed") +
  scale_y_continuous(name = "ALE")

p_ale_others <- wrap_plots(p_tp, p_hm, p_rd, p_ws, nrow = 2)

