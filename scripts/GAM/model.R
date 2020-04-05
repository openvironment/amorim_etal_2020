# GAM

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mgcv)
library(caret)
library(recipes)
library(patchwork)


# Data --------------------------------------------------------------------

tab_model <- read_rds("data/data_model.rds")
tab_bs <- read_rds("data/data_gas_200_bs.rds")


# Utils -------------------------------------------------------------------

gam_plot <- function(fit, smooth, xlab, ylab) {
  
  x <- fit$model[,smooth$term]
  
  if (smooth$by == "NA") {
    by.level = "NA"
  } else {
    by.level = smooth$by.level
  }
  
  range = tibble(x = x, by = by.level)
  names(range) = c(smooth$term, smooth$by)
  
  par <- smooth$first.para:smooth$last.para
  mat <- PredictMat(smooth, range)
  y <- (mat %*% fit$coefficients[par]) %>% as.numeric
  
  se <- ((mat %*% fit$Vp[par, par, drop = FALSE]) * mat) %>%
    rowSums %>% 
    sqrt
  
  df <- tibble(
    label = smooth$label,
    x.var = smooth$term,
    x.val = x,
    value = y,
    se = se
  )
  
  ggplot(df, aes(x.val, value)) +
    geom_ribbon(aes(ymin = value - 2*se, ymax = value + 2*se), 
                fill = "grey80") +
    geom_line(color = 'blue', size = 1) +
    #geom_point(aes(x = x.val, y = min(value-2*se)-sd(value))) +
    labs(y = ylab, x = xlab) +
    theme_bw()
  
}

pred_obs_plot <- function(obs, pred) {
  
  tibble(x = obs, y = pred) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "royal blue") +
    labs(x = "Observed", y = "Predicted") +
    theme_bw()
  
}

# Data prep ---------------------------------------------------------------

tab_model <- 
  recipe(tab_model) %>% 
  step_dummy(stationname) %>% 
  step_interact(
    terms = ~ matches("^stationname"):trend +
      matches("^stationname"):dv_beltway_open
  ) %>% 
  prep(training = tab_model) %>% 
  bake(new_data = tab_model)

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


# Fit Gaussian ------------------------------------------------------------

# Setting seeds for reproducibility
set.seed(5893524) # My student ID
seeds <- map(1:5, ~sample.int(1000, 2))
seeds[[6]] <- sample.int(1000, 1)

train_control <- trainControl(
  method = "cv", 
  number = 5,  
  verboseIter = TRUE,
  seeds = seeds
)

model <- train(
  form = formula,
  data = na.omit(tab_model),
  method = "gam",
  trControl = train_control
)

# model
# summary(model$finalModel)  
# varImp(model)

# Smooth term plot

p <- gam_plot(
  model$finalModel, 
  model$finalModel$smooth[[1]],
  xlab = "ShareE25",
  ylab = "Effect on the ozone concentration"
)

# Observed vs predicted plot

p_gaussian <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("(a)")


# Fit Gamma ---------------------------------------------------------------

model <- train(
  form = formula, 
  data = na.omit(tab_model), 
  method = "gam",
  family = Gamma(link = log),
  trControl = train_control
)

# model
# summary(model$finalModel)

# varImp(model$finalModel) %>%
#   rownames_to_column() %>%
#   arrange(desc(Overall))

# Observed vs predicted plot

p_gamma <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("(b)")


# Inverse Gaussian fit ----------------------------------------------------

model <- train(
  form = formula, 
  data = na.omit(df_model), 
  method = "gam",
  family = inverse.gaussian(link = "1/mu^2"),
  trControl = train_control
)

# model
# summary(model$finalModel) 
# varImp(model)

# Observed vs predicted plot

p_inv_gaussian <- pred_obs_plot(
  obs = na.omit(tab_model)$o3_mass_conc,
  pred = predict(model, newdata = na.omit(tab_model))
) +
  ggtitle("(c)")

