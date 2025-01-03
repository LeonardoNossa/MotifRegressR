#' @title Plot KNN Model Metrics and Feature Importance
#' @description Generates plots and data summaries for K-Nearest Neighbors (KNN)
#' model metrics (RMSE, R2, MAE) and feature importance.
#' @param KNN_model A trained KNN model object from the caret package.
#' @return A list containing data frames and ggplot objects
#' for RMSE, R2, MAE, and feature importance plots.
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal scale_y_continuous element_text geom_bar
#' @importFrom caret varImp
#' @importFrom scales pretty_breaks
#' @export
plot_KNN <- function(KNN_model){

  #PLOT 1 -- RMSE
  rmse_vec <- KNN_model$results$RMSE
  min_rmse_index <- which(rmse_vec == min(rmse_vec))
  rmse_data <- data.frame(Parameter = seq_along(rmse_vec),
                          RMSE = rmse_vec)

  rmse_plot <- ggplot2::ggplot(data = rmse_data, ggplot2::aes(x = as.factor(Parameter), y = RMSE)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point(color = "black", fill = "darkgray", size = 2, shape = 21) +
    ggplot2::geom_point(data = rmse_data[min_rmse_index, ],
                        ggplot2::aes(x = Parameter, y = RMSE),
                        color = "black", fill = "red", size = 3, shape = 21) +
    ggplot2::labs(title = "Selected model [Lowest RMSE]", x = "Number of Parameters", y = "RMSE") +
    ggplot2::scale_y_continuous(breaks = seq(min(rmse_data$RMSE), max(rmse_data$RMSE), length.out = 20)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  #PLOT 2 -- R2
  r2_vec <- KNN_model$results$Rsquared
  r2_data <- data.frame(Parameter = seq_along(r2_vec),
                        R2 = r2_vec)

  r2_plot <- ggplot2::ggplot(data = r2_data, ggplot2::aes(x = as.factor(Parameter), y = R2)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point(color = "black", fill = "darkgray", size = 2, shape = 21) +
    ggplot2::labs(title = "R2 plot", x = "Number of Parameters", y = "R2") +
    ggplot2::scale_y_continuous(breaks = seq(min(r2_data$R2), max(r2_data$R2), length.out = 20)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  #PLOT 3 -- MAE
  mae_vec <- KNN_model$results$MAE
  mae_data <- data.frame(Parameter = seq_along(mae_vec),
                         MAE = mae_vec)

  mae_plot <- ggplot2::ggplot(data = mae_data, ggplot2::aes(x = as.factor(Parameter), y = MAE)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point(color = "black", fill = "darkgray", size = 2, shape = 21) +
    ggplot2::labs(title = "MAE plot", x = "Number of Parameters", y = "MAE") +
    ggplot2::scale_y_continuous(breaks = seq(min(mae_data$MAE), max(mae_data$MAE), length.out = 20)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  #PLOT 4 - REDUCED MODEL IMPORTANCE VALUES
  importance <- caret::varImp(KNN_model$fit, scale = FALSE)$importance
  importance_df <- data.frame(
    Features = rownames(importance),
    Importance = importance$Overall
  )

  importance_plot <- ggplot2::ggplot(importance_df, ggplot2::aes(x = reorder(Features,Importance,decreasing = TRUE), y = Importance)) +
    ggplot2::geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.5, size = 6)
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
    ggplot2::labs(
      title = "Feature Importance [RFE Best Model]",
      x = "Motifs",
      y = "Importance"
    )

  output_list <- list(RMSE_data = rmse_data,
                      R2_data = r2_data,
                      MAE_data = mae_data,
                      importance_data = importance_df,
                      RMSE_plot = rmse_plot,
                      R2_plot = r2_plot,
                      MAE_plot = mae_plot,
                      importance_plot = importance_plot)

  return(output_list)
}


#' @title Plot Random Forest (RF) Model Metrics and Feature Importance
#' @description Generates plots and data summaries for
#' Random Forest model metrics (MSE, % Variance Explained) and motif importance.
#' @param RF_model A trained Random Forest model object.
#' @return A list containing metrics, ggplot objects
#' for MSE and explained variance plots, and feature importance data.
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal element_text
#' @importFrom stats reorder
#' @export
plot_RF <- function(RF_model) {

  output <- list()


  data <- data.frame(
    Index = seq_along(RF_model$mse),
    MSE = RF_model$mse,
    Expl_Var = RF_model$rsq
  )

  importance_data <- data.frame(
    Motif = rownames(RF_model$importance),
    Importance = RF_model$importance[, 1]
  )
  importance_data$Motif <- stats::reorder(importance_data$Motif,
                                          importance_data$Importance)


  mse_elbow_plot <- ggplot2::ggplot(data, ggplot2::aes(x = Index, y = MSE)) +
    ggplot2::geom_point(shape = 1, color = "darkblue") +
    ggplot2::labs(
      title = "Mean Squared Error",
      x = "# of trees",
      y = "MSE"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14)
    )

  rsq_elbow_plot <- ggplot2::ggplot(data, ggplot2::aes(x = Index, y = Expl_Var)) +
    ggplot2::geom_point(shape = 1, color = "darkred") +
    ggplot2::labs(
      title = "% of Variance Explained",
      x = "# of trees",
      y = "% of Var. Explained"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14)
    )

  importance_dotplot <- ggplot2::ggplot(importance_data, ggplot2::aes(x = Importance,
                                                                      y = Motif)) +
    ggplot2::geom_point(color = "purple", size = 3) +
    ggplot2::labs(
      title = "Motif Importance",
      x = "Importance",
      y = "Motif"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  output[["mse"]] <- RF_model$mse[length(RF_model$mse)]
  output[["mse_elbow_plot"]] <- mse_elbow_plot
  output[["percentage_Var_explained"]] <- RF_model$rsq[length(RF_model$rsq)]
  output[["percentage_Var_explained_elbow_plot"]] <- rsq_elbow_plot
  output[["importance"]] <- RF_model$importance
  output[["importance_dotplot"]] <- importance_dotplot

  return(output)

}


#' @title Plot Linear Model (LM) Diagnostics and Coefficients
#' @description Generates diagnostic plots and
#' coefficients data for a Linear Model (LM).
#' @param LM_model A trained linear model object.
#' @return A list containing data frames for
#' coefficients and recorded plots for diagnostic analysis.
#' @importFrom grDevices recordPlot dev.off
#' @export
plot_LM <- function(LM_model){

  coefficients_df <- data.frame(Coefficient = LM_model$coefficients)

  plot(LM_model, which = 1)
  residual_vs_fitted_plot <- grDevices::recordPlot()

  plot(LM_model, which = 2)
  qq_plot <- grDevices::recordPlot()

  plot(LM_model, which = 3)
  scale_location_plot <- grDevices::recordPlot()

  plot(LM_model, which = 4)
  cooks_distance_plot <- grDevices::recordPlot()

  plot(LM_model, which = 5)
  residual_vs_leverage_plot <- grDevices::recordPlot()

  plot(LM_model, which = 6)
  cooks_distance_vs_leverage_plot <- grDevices::recordPlot()

  dev.off()

  output_list <- list(coefficients_data = coefficients_df,
                      residual_vs_fitted_plot = residual_vs_fitted_plot,
                      qq_plot = qq_plot,
                      scale_location_plot = scale_location_plot,
                      cooks_distance_plot = cooks_distance_plot,
                      residual_vs_leverage_plot = residual_vs_leverage_plot,
                      cooks_distance_vs_leverage_plot = cooks_distance_vs_leverage_plot)

  return(output_list)
}


#' @title Plot SVM Model Feature Importance and Metrics
#' @description Generates feature importance bar plot and metrics
#' (MSE, R2, RMSE, MAE) for an SVM model.
#' @param SVM_model_sublist A list containing SVM model objects.
#' @param Compendium A data frame with actual values.
#' @param Scores A data frame with predicted scores.
#' @return A list containing a ggplot object for feature importance,
#' a data frame of feature importance, and a metrics data frame.
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs theme_minimal element_text
#' @importFrom stats predict
#' @export
plot_SVM <- function(SVM_model_sublist,Compendium,Scores){

  model <-  SVM_model_sublist[[1]]
  common <- intersect(rownames(Compendium),rownames(Scores))

  Compendium  <- Compendium[common,]
  Scores  <- Scores[common,]


  importance <- as.numeric(model$coefs) %*% as.matrix(model$SV)
  feature_importance <- apply(importance, 2, function(x) sqrt(sum(x^2)))
  feature_importance <- as.vector(feature_importance)

  importance_df <- data.frame(
    Feature = colnames(model$SV),
    Importance = feature_importance)

  p <- ggplot2::ggplot(importance_df, ggplot2::aes(x = reorder(Feature, Importance), y = Importance)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", fill = "purple") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Feature Importance for", names(SVM_model_sublist), 'Condition'),
      x = "Features",
      y = "Importance"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
    )

  importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), , drop = FALSE]
  rownames(importance_df) <- seq_along(1:nrow(importance_df))
  model_name <- names(SVM_model_sublist)

  predictions <- stats::predict(model, Scores)

  actual <- Compendium[[model_name]]

  mse <- mean((actual - predictions)^2)
  r2 <- 1 - (sum((actual - predictions)^2) / sum((actual - mean(actual))^2))
  rmse <- sqrt(mean((actual - predictions)^2))
  mae <- mean(abs(actual - predictions))

  metrics_df <- data.frame(
    MSE = mse,
    R2 = r2,
    RMSE = rmse,
    MAE = mae
  )
  return(list(plot = p,
              df = importance_df,
              metrics = metrics_df))

}


#' @title Plot Partial Least Squares (PLS) Model Coefficients and Validation
#' @description Generates component coefficient plots, heatmaps,
#' and validation plots for a PLS model.
#' @param PLS_model A trained PLS model object from the pls package.
#' @return A list containing ggplot/plotly plots
#' and data frames for PLS coefficients and validation.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_minimal geom_tile scale_fill_gradient2
#' @importFrom plotly ggplotly layout style
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom pls validationplot
#' @importFrom grDevices recordPlot dev.off
#' @export
plot_PLS <- function(PLS_model){

  library(pls)

  `%>%` <- magrittr::`%>%`


  df <- as.data.frame(PLS_model$coefficients)
  coefficients_transposed <- t(df)
  coefficients_long <- reshape2::melt(coefficients_transposed)
  colnames(coefficients_long) <- c("Component", "Motif", "Coefficient")

  component_number <- as.numeric(gsub("^[^0-9]*([0-9]+).*", "\\1", coefficients_long$Component))
  component_number <- paste0("Component_",component_number)
  coefficients_long$Component <- component_number

  output_list <- list()
  for (component in unique(coefficients_long$Component)) {

    sub_df <- coefficients_long[coefficients_long$Component == component,]

    plot <- ggplot2::ggplot(sub_df, ggplot2::aes(x = Motif, y = Coefficient)) +
      ggplot2::geom_bar(aes(fill = Coefficient > 0), stat = "identity", position = "dodge", color = "black") +
      suppressWarnings(ggplot2::geom_point(ggplot2::aes(
        y = Coefficient,
        text = paste0("Motif: ", Motif, "<br>Coefficient: ", Coefficient)
      ), color = "transparent")) +
      ggplot2::scale_fill_manual(values = c("lightcoral", "lightblue")) +
      ggplot2::labs(x = "Motifs", y = "Coefficients") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(),
        legend.position = "none"
      ) +
      ggplot2::facet_wrap(~Component, scales = "free_x")

    plotly_plot <- plotly::ggplotly(plot, tooltip = c("text"))

    plotly_plot <- plotly_plot %>%
      plotly::layout(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(
            color = "black"
          )
        )
      ) %>%
      plotly::style(hoverinfo = "none", traces = 1)

    output_list[[component]] <- plotly_plot

  }

  heatmap <- ggplot2::ggplot(coefficients_long, ggplot2::aes(x = Component, y = Motif, fill = Coefficient)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    ggplot2::labs(x = "Components", y = "Motifs", fill = "Coefficient") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5),
                   axis.text.x = ggplot2::element_text(vjust = 0.45,angle = 45),
                   panel.grid = ggplot2::element_blank())

  output_list[["heatmap"]] <- heatmap

  pls::validationplot(PLS_model)
  validation_plot <- grDevices::recordPlot()
  dev.off()

  output_list[["validation_plot"]] <- validation_plot
  output_list[["coefficients_df"]] <- coefficients_long

  return(output_list)

}



#' @title Plot Random LASSO Model Stability and Selection Probability
#' @description Generates stability paths and selection
#' probability plots for a Random LASSO model.
#' @param RANDLASSO_model A trained Random LASSO model
#' object from the monaLisa package.
#' @return A list containing stability paths,
#' selection probability plots, and selection probability data.
#' @importFrom monaLisa plotStabilityPaths plotSelectionProb
#' @importFrom grDevices recordPlot dev.off
#' @export
plot_RANDLASSO <- function(RANDLASSO_model){

  df <- data.frame(Prob = RANDLASSO_model$selProb,
                   AUC = RANDLASSO_model$selAUC)

  monaLisa::plotStabilityPaths(RANDLASSO_model)
  stability_paths <- grDevices::recordPlot()

  monaLisa::plotSelectionProb(RANDLASSO_model, directional = TRUE)
  selection_prob <- grDevices::recordPlot()

  dev.off()

  return(list(prob_df = df,
              stability_paths = stability_paths,
              selection_prob = selection_prob))

}



#' @title Plot O2PLS Model Metrics and Feature Importance
#' @description Generates plots and data summaries for
#' O2PLS model metrics (motifs weights and correlation between joint X scores
#' and joint Y scores).
#' @param O2PLS_model A trained O2PLS model object.
#' @return A list containing motifs weights and ggplot objects
#' for such weights and correlation between joint X scores and joint Y scores.
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal element_text
#' @importFrom stats reorder
#' @export
plot_O2PLS <- function(O2PLS_model) {

  output <- list()

  joint_X_loadings_df <- data.frame(
    Motifs = rownames(O2PLS_model$W.),
    Loadings = O2PLS_model$W.[, 1]
  )

  joint_X_loadings_df <- joint_X_loadings_df[order(joint_X_loadings_df$Loadings,
                                                   decreasing = TRUE), ]

  joint_X_loadings_dotchart <- ggplot2::ggplot(joint_X_loadings_df,
                                               ggplot2::aes(x = Loadings,
                                                            y = stats::reorder(Motifs, Loadings))) +
    ggplot2::geom_point(color = "blue", size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Dotchart Joint X Loadings",
      x = "Loadings",
      y = "Motifs"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18),
                   axis.text.y = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 16, vjust = 1),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16))


  output[["joint_X_loadings"]] <- O2PLS_model$W.
  output[["joint_X_loadings_dotchart"]] <- joint_X_loadings_dotchart


  scatter_df <- data.frame(
    Joint_X_Scores = O2PLS_model$Tt[, 1],
    Joint_Y_Scores = O2PLS_model$U[, 1]
  )

  scatterplot_joint_scores <- ggplot2::ggplot(scatter_df, ggplot2::aes(x = Joint_X_Scores, y = Joint_Y_Scores)) +
    ggplot2::geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Scatterplot Joint X Scores vs Joint Y Scores",
      x = "Joint X Scores",
      y = "Joint Y Scores"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 10)
    )


  output[["scatterplot_joint_scores"]] <- scatterplot_joint_scores
  return(output)
}

