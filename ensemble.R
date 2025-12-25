# -------- LIBRARIES AND SEED --------
library(dplyr)
set.seed(1234)
options(warn = -1)

# -------- DATASET READ AND CLEAN --------
# read dataset from Nathalia Oliveira et al. (2023)
dataset_natalia <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Saraiva 2023",
  "Tabelas do Relatorio - Nathalia Oliveira (Agnaldo Lopes) - marco 2023.xlsx")
)

# drop header rows
dataset_natalia <- dataset_natalia[-c(1, 2), , drop = FALSE]
colnames(dataset_natalia) <- as.character(unlist(dataset_natalia[1, ]))
dataset_natalia <- dataset_natalia[-1, ]

# rename and convert variables to numeric
dataset_natalia <- dataset_natalia |>
  rename(
    Idade = `Idade (anos)`,
    Peso = `Peso (kg)`,
    Altura = `Altura (m)`,
    IMC = `IMC (kg/m2)`,
    FC = `FC antes (bpm)`,
    Borg = `BORG 6'`,
    `6MWD` = `Distância TC6' (m)`
  ) |>
  dplyr::mutate(dplyr::across(
    c(Idade, Peso, Altura, IMC, FC, Borg, `6MWD`),
    as.numeric
  ))

# define variables per protocol
dataset_natalia <- dataset_natalia |>
  dplyr::mutate(
    Sexo = "female",
    Corredor = 30
  )

# select only relevant variables
dataset_natalia <- dataset_natalia %>%
  dplyr::select(Corredor, Sexo, Idade, Peso, Altura, IMC, FC, Borg, `6MWD`)

table1::label(dataset_natalia$Corredor) <- "m"
table1::label(dataset_natalia$Idade) <- "anos"
table1::label(dataset_natalia$Peso) <- "kg"
table1::label(dataset_natalia$Altura) <- "m"
table1::label(dataset_natalia$IMC) <- "kg/m²"
table1::label(dataset_natalia$FC) <- "bpm"
table1::label(dataset_natalia$Borg) <- "unidade"
table1::label(dataset_natalia$`6MWD`) <- "m"

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
available_vars <- colnames(dataset_natalia)
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
B <- 10000

# initialize results dataframe
results_ensemble <- data.frame(
  b = 1:B,
  dataset = rep(NA, B),
  n_models = rep(NA, B),
  models = rep(NA, B),
  distance_meas = rep(NA, B),
  distance_est = rep(NA, B),
  bias = rep(NA, B),
  lower_CI = rep(NA, B),
  upper_CI = rep(NA, B)
)

# Monte Carlo over model space (bootstrap samples of participants)
for (b in 1:B) {
  # print b every 100 steps
  if (b %% 100 == 0) {
    cat("Bootstrap samples completed:", b, "\n")
  }
  
  # bootstrap resample dataset
  resampled_idx <- sample(seq_len(nrow(dataset_natalia)),
                          replace = TRUE)
  dataset_boot <- dataset_natalia[resampled_idx, ]
  
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
      dplyr::select(`Variable (PT)`, `Unit`, `Exponent`, dplyr::all_of(model_name)) %>%
      dplyr::filter(!is.na(.data[[model_name]]))
    
    # compute predictions for each subject
    for (j in 1:nrow(dataset_boot)) {
      pred <- numeric(1)
      for (k in 2:nrow(model_coeffs)) { # starts with 2 because of sex code female
        var_name <- model_coeffs$`Variable (PT)`[k]
        unit <- model_coeffs$Unit[k]
        exponent <- as.numeric(model_coeffs$Exponent[k])
        coeff <- as.numeric(model_coeffs[[model_name]][k])
        if (var_name == "Intercepto") {
          pred <- pred + coeff
        } else if (var_name == "Sexo") {
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
        } else {
          # get variable value from dataset
          var_value <- dataset_boot[[var_name]][j]
          # check for unit conversion
          if(var_name == "Altura"){
            # unit conversion if needed
            if (unit == "cm" & var_value < 100) {
              var_value <- var_value * 100
            }
            if (unit == "m" & var_value > 10) {
              var_value <- var_value / 100
            }
          }
          pred <- pred + coeff * (var_value ^ exponent)
        }
      }
      predictions_matrix[j, i] <- pred
    }
  }
  # compute ensemble predictions as mean of model predictions
  results_ensemble$distance_est[b] <- mean(predictions_matrix, na.rm = TRUE)
  
  # compute bias based on individual participant data and average measured distance across models
  individual_biases <- rowMeans(predictions_matrix, na.rm = TRUE) - dataset_boot$`6MWD`
  results_ensemble$bias[b] <- mean(individual_biases, na.rm = TRUE)
  
  # compute 95% CI for bias (percentile method)
  results_ensemble$lower_CI[b] <- quantile(individual_biases, 0.025, na.rm = TRUE)
  results_ensemble$upper_CI[b] <- quantile(individual_biases, 0.975, na.rm = TRUE)
}

View(results_ensemble)
# save results
write.csv(
  results_ensemble,
  file.path(getwd(), "results", "ensemble_results_natalia_oliveira_2023.csv"),
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
p_overlay <- ggplot2::ggplot() +
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
      "Lower CI (min)" = "blue",
      "Lower CI (max)" = "blue",
      "Upper CI (min)" = "red",
      "Upper CI (max)" = "red"
    )
  ) +
  ggplot2::scale_fill_manual(
    name = "CI Envelopes",
    values = c(
      "Lower 95% CI range" = "blue",
      "Upper 95% CI range" = "red"
    )
  ) +
  ggplot2::scale_linetype_manual(
    name = "",
    values = c("Zero reference" = "solid")
  ) +
  ggplot2::labs(
    title = "Bias and 95% CI Envelopes vs Number of Models",
    x = "Number of Models",
    y = "Bias (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "right"
  )

print(p_overlay)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), "results", "Figure 2.tiff"),
  plot = p_overlay,
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

View(best_ensembles)

# save best ensembles
write.csv(
  best_ensembles,
  file.path(getwd(), "results", "best_ensembles_natalia_oliveira_2023.csv"),
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

print(p2)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), "results", "Figure 3.tiff"),
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
    ggplot2::geom_hline(yintercept = 0, color = "black")
  
  plot_list[[as.character(n_models)]] <- p
}

# Arrange plots in a grid
multipanel_plot <- gridExtra::grid.arrange(
  grobs = plot_list,
  ncol = 6,
  top = "Ensemble Bias by Number of Models"
)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), "results", "Figure 4.tiff"),
  plot = multipanel_plot,
  device = "tiff",
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# Select best subset of models to plot IPD measured vs predicted
best_overall <- results_ensemble %>%
  dplyr::slice_min(order_by = abs(bias), n = 1)
best_model_names <- unlist(strsplit(best_overall$models, "; "))
# recompute predictions matrix for best overall ensemble
predictions_matrix_best <- matrix(NA, nrow = nrow(dataset_natalia),
                                  ncol = length(best_model_names))

