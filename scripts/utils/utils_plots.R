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
