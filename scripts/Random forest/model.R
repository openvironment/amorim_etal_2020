# Random forest

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
  splitrule = "variance",
  mtry = 60,
  min.node.size = 3
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
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

# model
# model$finalModel
# varImp(model)

# Observed vs predicted plot
p <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
)


# Interpretation ----------------------------------------------------------

df_train <- rec %>%
  prep(tab_model) %>% 
  bake(tab_model)

X <- df_train %>% 
  select(-o3_mass_conc) %>% 
  as.matrix()

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
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  importance = 'impurity'
)

predictor <- Predictor$new(model, data = df_train, y = df_train$o3_mass_conc)

# ALE plot
ale <- FeatureEffect$new(predictor, feature = "share_gas", grid.size = 20)

p_ale <- ale$plot() + 
  theme_bw() +
  labs(x = "shareE25") +
  scale_y_continuous(name = "ALE") +
  ggtitle("Random forest")
