# We developed a Monte Carlo feasible model averaging approach with individual-level bootstrap (MC-FMA-IPD)
# in which random subsets of compatible published prediction equations are repeatedly sampled, 
# aggregated, and evaluated across bootstrap resamples of individual participant data.

set.seed(1234)

# -------- DATASET READ AND CLEAN --------
# read dataset from Nathalia Oliveira et al. (2023)
# source("saraiva_2023.R")
# read dataset from Pinto et al. (2019)
# source("pinto_2019.R")

# -------- MODEL READ --------
# read model parameters
rawmodels <- readxl::read_xlsx(file.path(getwd(), "models.xlsx"), col_names = FALSE)
models <- paste0("[", sprintf("%02d", 1:(ncol(rawmodels) - 4)), "] ", rawmodels[1, c(-1:-4)])
colnames(rawmodels) <- c(
  "Variable",
  "Variable (PT)",
  "Unit",
  "Exponent",
  models
)
model_params <- rawmodels[-1, ]

# dataset compatibility: keep only models with scope = female or both
sex_scope <- model_params |>
  dplyr::filter(.data[["Variable"]] == "Sex scope") |>
  dplyr::select(-1) |>
  unlist(use.names = FALSE) |>
  as.character()

cols_keep <- which(sex_scope %in% c("female", "both")) + 1

model_params <- model_params |>
  dplyr::select(1:4, all_of(cols_keep))

model_params <- model_params |>
  dplyr::slice(-1:-7)

# -------- SELECT EQUATIONS BASED ON AVAILABLE PARAMETERS --------

# identify available predictors in dataset
available_vars <- colnames(dataset)
available_vars <- c(available_vars, "Intercepto")

# identify model columns (excluding metadata)
model_cols <- colnames(model_params)[-(1:4)]

# logical vector indicating if model is compatible
model_is_compatible <- sapply(model_cols, function(model_name) {
  
  vars_in_model <- model_params |>
    dplyr::select(`Variable (PT)`, all_of(model_name)) |>
    dplyr::filter(!is.na(.data[[model_name]])) |>
    dplyr::pull(`Variable (PT)`) |>
    unique()
  
  # check if all required variables are available
  all(vars_in_model %in% available_vars)
})

# keep only compatible models
model_params_filtered <- model_params |>
  dplyr::select(
    1:4,
    dplyr::all_of(model_cols[model_is_compatible])
  )

model_params_filtered <- model_params_filtered[
  model_params_filtered$`Variable (PT)` %in% available_vars,
]

# add sex code female as first row again
model_params_filtered <- rawmodels[which(rawmodels$Variable == "Sex code female"), ] |>
  dplyr::select(1:4, dplyr::all_of(model_cols[model_is_compatible])) |>
  dplyr::bind_rows(model_params_filtered)

# -------- ENSEMBLE PREDICTION --------

# Bootstrap samples
# B <- 1000

# initialize results dataframe
results_ensemble <- data.frame(
  b = 1:B,
  dataset = rep(NA, B),
  n_models = rep(NA, B),
  models = rep(NA, B),
  distance_meas = rep(NA, B),
  distance_est = rep(NA, B),
  r2 = rep(NA, B),
  bias = rep(NA, B),
  lower_CI = rep(NA, B),
  upper_CI = rep(NA, B),
  mae = rep(NA, B),
  rmse = rep(NA, B)
)

# Monte Carlo Model Averaging com bootstrap em IPD
# Monte Carlo ensemble across the feasible model subspace
for (b in 1:B) {
  # print b every 100 steps
  if (b %% 100 == 0) {
    cat("Bootstrap samples completed:", b, "\n")
  }
  
  # bootstrap resample dataset
  resampled_idx <- sample(seq_len(nrow(dataset)),
                          replace = TRUE)
  dataset_boot <- dataset[resampled_idx, ]
  
  # dataset name
  results_ensemble$dataset[b] <- "natalia_oliveira_2023"
  
  # store mean measured 6MWD
  results_ensemble$distance_meas[b] <- mean(dataset_boot$`6MWD`, na.rm = TRUE)
  
  # sort number of models used
  n_models_used <- sample(1:ncol(model_params_filtered[-c(1:4)]), 1)
  results_ensemble$n_models[b] <- n_models_used
  
  # sample n models
  sampled_models <- sample(
    colnames(model_params_filtered[-c(1:4)]),
    n_models_used,
    replace = FALSE
  )
  results_ensemble$models[b] <- paste(sampled_models, collapse = "; ")
  
  # initialize predictions matrix
  predictions_matrix <- matrix(NA, nrow = nrow(dataset_boot), ncol = n_models_used)
  colnames(predictions_matrix) <- sampled_models
  
  # loop over sampled models
  for (i in 1:n_models_used) {
    model_name <- sampled_models[i]
    
    model_coeffs <- model_params_filtered %>%
      dplyr::select(`Variable`, `Variable (PT)`, `Unit`, `Exponent`, dplyr::all_of(model_name)) %>%
      dplyr::filter(!is.na(.data[[model_name]]))
    
    # check if "Intercepto" is in model
    intercept <- 0
    if ("Intercepto" %in% model_coeffs$`Variable (PT)`) {
      intercept <- as.numeric(model_coeffs %>%
                                dplyr::filter(`Variable (PT)` == "Intercepto") %>%
                                dplyr::select(dplyr::all_of(model_name)))
      # remove intercept from model_coeffs to avoid double counting
      model_coeffs <- model_coeffs %>%
        dplyr::filter(`Variable (PT)` != "Intercepto")
    }
    
    # check if sex code female is in model_coeffs to add start from 2 or 1
    has_code <- "Sex code female" %in% model_coeffs$Variable
    start <- ifelse(has_code, 2, 1)
    
    # compute predictions for each subject
    for (j in 1:nrow(dataset_boot)) {
      # initialize prediction
      pred <- numeric(1)
      pred <- pred + intercept
      
      for (k in start:nrow(model_coeffs)) { # starts with 2 because of sex code female
        var_name <- model_coeffs$`Variable (PT)`[k]
        unit <- model_coeffs$Unit[k]
        exponent <- as.numeric(model_coeffs$Exponent[k])
        coeff <- as.numeric(model_coeffs[[model_name]][k])
        if (var_name == "Sexo") {
          # get participant sex value
          var_value <- dataset_boot[[var_name]][j]
          # get sex code female value
          female_code <- as.numeric(model_coeffs %>%
                                      dplyr::filter(`Variable (PT)` == "Sex code female") %>%
                                      dplyr::select(dplyr::all_of(model_name)))
          # assign 1 or 0 based on female code
          if (var_value == "female") {
            var_value <- female_code
          } else {
            if(female_code == 1){
              var_value <- 0
            } else {
              var_value <- 1
            }
          }
          pred <- pred + coeff * (var_value ^ exponent)
        } else if (var_name == "Idade" && exponent == -1) {
          # use ln(age)
          var_value <- log(dataset_boot[[var_name]][j])
          pred <- pred + coeff * var_value
        } else {
          # get variable value from dataset
          var_value <- dataset_boot[[var_name]][j]
          # check for unit conversion
          if(var_name == "Altura"){
            # unit conversion if needed
            if (unit == "cm" && var_value < 100) {
              var_value <- var_value * 100
            }
            if (unit == "m" && var_value > 10) {
              var_value <- var_value / 100
            }
          }
          pred <- pred + coeff * (var_value ^ exponent)
        }
      }
      predictions_matrix[j, i] <- pred
    }
  }
  
  # compute bias based on individual participant data and average measured distance across models
  ensemble_pred_subject <- rowMeans(predictions_matrix, na.rm = TRUE)
  results_ensemble$distance_est[b] <- mean(ensemble_pred_subject, na.rm = TRUE)
  
  # compute R² (Bootstrap distribution of explained variance / rank preservation)
  r2_b <- cor(
    ensemble_pred_subject,
    dataset_boot$`6MWD`,
    use = "complete.obs"
  )^2
  results_ensemble$r2[b] <- r2_b
  
  individual_biases <- ensemble_pred_subject - dataset_boot$`6MWD`
  results_ensemble$bias[b] <- mean(individual_biases, na.rm = TRUE)
  
  # compute 95% CI for bias (percentile method)
  results_ensemble$lower_CI[b] <- quantile(individual_biases, 0.025, na.rm = TRUE)
  results_ensemble$upper_CI[b] <- quantile(individual_biases, 0.975, na.rm = TRUE)
  
  # compute MAE and RMSE
  results_ensemble$mae[b] <- mean(abs(individual_biases), na.rm=TRUE)
  results_ensemble$rmse[b] <- sqrt(mean(individual_biases^2, na.rm=TRUE))
}

# save results
write.csv(
  results_ensemble,
  file.path(getwd(), results_folder, "ensemble_results.csv"),
  row.names = FALSE
)

ci_envelope_sep <- results_ensemble %>%
  dplyr::group_by(n_models) %>%
  dplyr::summarise(
    lower_min = min(lower_CI),
    lower_max = max(lower_CI),
    upper_min = min(upper_CI),
    upper_max = max(upper_CI)
  ) %>%
  dplyr::ungroup()

# plot results (two panels)
# bias vs number of models
p1 <- ggplot2::ggplot() +
  # LOWER CI envelope
  ggplot2::geom_ribbon(
    data = ci_envelope_sep,
    ggplot2::aes(
      x = n_models,
      ymin = lower_min,
      ymax = lower_max,
      fill = "Lower 95% CI range"
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    data = ci_envelope_sep,
    ggplot2::aes(x = n_models, y = lower_min, color = "Lower CI (min)"),
    linewidth = 0.9
  ) +
  ggplot2::geom_line(
    data = ci_envelope_sep,
    ggplot2::aes(x = n_models, y = lower_max, color = "Lower CI (max)"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  # UPPER CI envelope
  ggplot2::geom_ribbon(
    data = ci_envelope_sep,
    ggplot2::aes(
      x = n_models,
      ymin = upper_min,
      ymax = upper_max,
      fill = "Upper 95% CI range"
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    data = ci_envelope_sep,
    ggplot2::aes(x = n_models, y = upper_min, color = "Upper CI (min)"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = ci_envelope_sep,
    ggplot2::aes(x = n_models, y = upper_max, color = "Upper CI (max)"),
    linewidth = 0.9
  ) +
  # Bias points and trend
  ggplot2::geom_point(
    data = results_ensemble,
    ggplot2::aes(x = n_models, y = bias, color = "Bias"),
    size = 2
  ) +
  ggplot2::geom_line(
    data = results_ensemble,
    ggplot2::aes(x = n_models, y = bias, color = "Bias mean"),
    stat = "summary",
    fun = mean,
    linewidth = 1,
    color = "black"
  ) +
  # Reference line
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = 0, linetype = "Zero reference"),
    color = "black"
  ) +
  # Scales
  ggplot2::scale_color_manual(
    name = "Components",
    values = c(
      "Bias" = "black",
      "Bias trend" = "black",
      "Upper CI (max)" = "red",
      "Upper CI (min)" = "red",
      "Lower CI (min)" = "blue",
      "Lower CI (max)" = "blue"
    )
  ) +
  ggplot2::scale_fill_manual(
    name = "CI Envelopes",
    values = c(
      "Upper 95% CI range" = "red",
      "Lower 95% CI range" = "blue"
    )
  ) +
  ggplot2::scale_linetype_manual(
    name = "",
    values = c("Zero reference" = "solid")
  ) +
  ggplot2::labs(
    title = "Bias and 95% CI Envelopes vs Number of Models",
    # bias and 95%CI
    subtitle = paste0("Bias: " ,
                      round(mean(results_ensemble$bias, na.rm = TRUE), 0), " m; ",
                      "95% CI: [",
                      round(mean(results_ensemble$lower_CI, na.rm = TRUE), 0), ", ",
                      round(mean(results_ensemble$upper_CI, na.rm = TRUE), 0), "] m"),
    x = "Number of Models",
    y = "Bias (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "right"
  )

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 2.tiff"),
  plot = p1,
  device = "tiff",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# select best ensemble (lowest absolute bias) for each number of models
best_ensembles <- results_ensemble %>%
  dplyr::group_by(n_models) %>%
  dplyr::slice_min(order_by = abs(bias), n = 1)

# save best ensembles
write.csv(
  best_ensembles,
  file.path(getwd(), results_folder, "best_ensembles.csv"),
  row.names = FALSE
)

# plot best ensembles
p2 <- ggplot2::ggplot(best_ensembles, ggplot2::aes(x = n_models, y = bias)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(
    title = "Best Ensemble Bias vs Number of Models",
    x = "Number of Models",
    y = "Bias (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank()) +
  ggplot2::geom_hline(yintercept = 0, color = "black") +
  ggplot2::geom_vline(xintercept = 0, color = "black") + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_CI, ymax = upper_CI), width = 0.2)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 3.tiff"),
  plot = p2,
  device = "tiff",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# Multipanel plot:
unique_n_models <- sort(unique(results_ensemble$n_models))
plot_list <- list()
for (n_models in unique_n_models) {
  
  subset_data <- results_ensemble %>%
    dplyr::filter(n_models == !!n_models) %>%
    dplyr::arrange(desc(bias)) %>%
    dplyr::mutate(model_id = dplyr::row_number())
  
  # remove duplicates in models
  subset_data <- subset_data %>%
    dplyr::distinct(models, .keep_all = TRUE)
  
  p <- ggplot2::ggplot(
    subset_data,
    ggplot2::aes(x = model_id, y = bias)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = paste(n_models, "Model(s)"),
      x = NULL,
      y = "Bias (m)"
    ) +
    ggplot2::theme_minimal(base_size = 8) + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    # get min max of y axis to apply across panels
    ggplot2::ylim(
      min(results_ensemble$bias, na.rm = TRUE) - 5,
      max(results_ensemble$bias, na.rm = TRUE) + 5
    )
  
  plot_list[[as.character(n_models)]] <- p
}

# Arrange plots in a grid
p3 <- gridExtra::grid.arrange(
  grobs = plot_list,
  ncol = 5,
  top = "Ensemble Bias by Number of Models"
)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 4.tiff"),
  plot = p3,
  device = "tiff",
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

error_summary <- results_ensemble %>%
  dplyr::group_by(n_models) %>%
  dplyr::summarise(
    mae_mean  = mean(mae, na.rm = TRUE),
    rmse_mean = mean(rmse, na.rm = TRUE)
  )

error_summary_ci <- results_ensemble %>%
  dplyr::group_by(n_models) %>%
  dplyr::summarise(
    mae_mean  = mean(mae, na.rm = TRUE),
    mae_lwr   = quantile(mae, 0.025, na.rm = TRUE),
    mae_upr   = quantile(mae, 0.975, na.rm = TRUE),
    rmse_mean = mean(rmse, na.rm = TRUE),
    rmse_lwr  = quantile(rmse, 0.025, na.rm = TRUE),
    rmse_upr  = quantile(rmse, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot2::ggplot(error_summary_ci, ggplot2::aes(x = n_models)) +
  ggplot2::geom_line(ggplot2::aes(y = mae_mean, color = "MAE"), linewidth = 1) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = mae_lwr, ymax = mae_upr, fill = "MAE"),
    alpha = 0.25
  ) +
  ggplot2::geom_line(ggplot2::aes(y = rmse_mean, color = "RMSE"), linewidth = 1) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = rmse_lwr, ymax = rmse_upr, fill = "RMSE"),
    alpha = 0.25
  ) +
  ggplot2::labs(
    title = "Individual Prediction Error vs Number of Models",
    x = "Number of Models",
    y = "Error (m)",
    color = "Metric",
    fill  = "Metric"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::geom_hline(yintercept = 0, color = "black") +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 5.tiff"),
  plot = p4,
  device = "tiff",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

p5 <- ggplot2::ggplot(results_ensemble,
                      ggplot2::aes(x = bias, y = rmse, color = n_models)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::scale_color_viridis_c() +
  ggplot2::labs(
    title = "Bias–Error Trade-off Across Ensembles",
    x = "Bias (m)",
    y = "RMSE (m)",
    color = "Number of Models"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::xlim(
    -max(abs(results_ensemble$bias), na.rm = TRUE),
    max(abs(results_ensemble$bias), na.rm = TRUE)
  )

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 6.tiff"),
  plot = p5,
  device = "tiff",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
