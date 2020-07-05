ale_plot <- function(data, model, feature, predict_function, grid.size, 
                     xlab, nrow = NULL) {
  
  predictor <- Predictor$new(
    model = model, 
    predict.function = predict_function,
    data = data, 
    y = "o3_mass_conc"
  )
  
  plots <- suppressMessages(map(
    grid.size,
    ~ FeatureEffect$new(
      predictor = predictor, 
      feature = feature,
      method = "ale",
      grid.size = .x
    )$plot() +
      labs(x = xlab, title = paste("grid:", .x)) +
      scale_y_continuous(name = "ALE")
  ))
  
  patchwork::wrap_plots(plots, nrow = nrow)
  
}


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

extract_gam_metrics <- function(tab, v) {
  tibble(
    .metric = c("rmse", "rsq", "mae"),
    .estimator = "standard",
    mean = c(tab$RMSE, tab$Rsquared, tab$MAE),
    n = v,
    std_err  = c(
      tab$RMSESD, 
      tab$RsquaredSD, 
      tab$MAESD
    ) / sqrt(v)
  )
}

extract_gam_estimates <- function(tab) {
  tab$p.table %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("term") %>%
    bind_rows(
      tibble(
        term = rownames(as.data.frame(tab$s.table)),
        Estimate = NA,
        `Std. Error` = NA,
        `t value` = as.data.frame(tab$s.table)$F,
        `Pr(>|t|)` = as.data.frame(tab$s.table)$`p-value`
      )
    ) %>% 
    rename(
      estimate = Estimate,
      std.error = `Std. Error`,
      statistic = `t value`,
      p.value = `Pr(>|t|)`
    )
}