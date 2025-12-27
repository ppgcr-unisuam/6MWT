# read results
results_ensemble <- read.csv(file.path(getwd(), results_folder, "ensemble_results.csv"))
best_ensembles <- read.csv(file.path(getwd(), results_folder, "best_ensembles.csv"))

ci_envelope_sep <- results_ensemble %>%
  dplyr::group_by(n_models) %>%
  dplyr::summarise(
    lower_min = min(lower_CI),
    lower_max = max(lower_CI),
    upper_min = min(upper_CI),
    upper_max = max(upper_CI)
  ) %>%
  dplyr::ungroup()

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
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(ci_envelope_sep$n_models))
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

offset <- 0.15

p4 <- ggplot2::ggplot(
  error_summary_ci,
  ggplot2::aes(x = as.numeric(factor(n_models)))
) +
  ggplot2::geom_line(
    ggplot2::aes(y = mae_mean, color = "MAE", group = 1),
    linewidth = 1,
    position = ggplot2::position_nudge(x = -offset)
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = mae_lwr,
      ymax = mae_upr,
      color = "MAE"
    ),
    width = 0.15,
    linewidth = 0.6,
    position = ggplot2::position_nudge(x = -offset)
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = rmse_mean, color = "RMSE", group = 1),
    linewidth = 1,
    position = ggplot2::position_nudge(x = offset)
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = rmse_lwr,
      ymax = rmse_upr,
      color = "RMSE"
    ),
    width = 0.15,
    linewidth = 0.6,
    position = ggplot2::position_nudge(x = offset)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(unique(error_summary_ci$n_models)),
    labels = unique(error_summary_ci$n_models)
  ) +
  ggplot2::labs(
    title = "Individual Prediction Error vs Number of Models",
    x = "Number of Models",
    y = "Error (m)",
    color = "Metric"
  ) +
  ggplot2::geom_hline(yintercept = 0, color = "black") +
  ggplot2::theme_minimal() +
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

p5 <- ggplot2::ggplot(
  results_ensemble,
  ggplot2::aes(
    x = bias,
    y = rmse,
    color = factor(n_models)
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::scale_color_viridis_d() +
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

# lookup table
model_lookup <- tibble::tibble(
  model_id    = sprintf("%02d", seq_along(models)),
  model_label = as.character(models)
)

# select only single models to build a forest plot
single_model_results <- results_ensemble %>%
  dplyr::filter(n_models == 1) %>%
  dplyr::mutate(
    model_name = stringr::str_extract(models, "\\[\\d+\\]"),
    model_name = stringr::str_remove_all(model_name, "\\[|\\]")
  ) %>%
  dplyr::select(
    model_name,
    bias,
    lower_CI,
    upper_CI
  ) %>%
  dplyr::distinct()

# Build lookup table: model index -> model name
model_lookup <- data.frame(
  model_id = sprintf("%02d", 1:(length(models))),
  model_label = as.character(models)
)

# select single models
single_model_results <- results_ensemble %>%
  dplyr::filter(n_models == 1) %>%
  dplyr::mutate(
    model_id = stringr::str_extract(models, "\\d+")
  ) %>%
  dplyr::left_join(model_lookup, by = "model_id") %>%
  dplyr::transmute(
    model_name_full = model_label,
    bias,
    lower_CI,
    upper_CI,
    bias_CI95 = sprintf(
      "%.2f [%.2f, %.2f]",
      bias, lower_CI, upper_CI
    )
  ) %>%
  dplyr::distinct()

single_model_results_plot <- single_model_results %>%
  dplyr::group_by(model_name_full) %>%
  dplyr::summarise(
    bias     = mean(bias, na.rm = TRUE),
    lower_CI = mean(lower_CI, na.rm = TRUE),
    upper_CI = mean(upper_CI, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(
    bias_CI95 = sprintf(
      "%.2f [%.2f, %.2f]",
      bias, lower_CI, upper_CI
    )
  )

mean_row <- single_model_results_plot %>%
  dplyr::summarise(
    model_name_full = "Mean bias (all single models)",
    bias = mean(bias, na.rm = TRUE),
    se   = sd(bias, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::mutate(
    lower_CI = bias - 1.96 * se,
    upper_CI = bias + 1.96 * se,
    bias_CI95 = sprintf(
      "%.2f [%.2f, %.2f]",
      bias, lower_CI, upper_CI
    )
  ) %>%
  dplyr::select(model_name_full, bias, lower_CI, upper_CI, bias_CI95)

# add last row for overall bias and 95%CI
single_model_results_plot <- single_model_results_plot %>%
  dplyr::bind_rows(mean_row)

# determine label x position
label_x <- max(single_model_results_plot$upper_CI, na.rm = TRUE) * 1.10

# separar último modelo
main_models <- single_model_results_plot %>% slice(-n())
last_model  <- single_model_results_plot %>% slice(n())

# reordenar apenas os principais
main_models <- main_models %>%
  arrange(bias)

# reconstruir data.frame
single_model_results_plot2 <- bind_rows(main_models, last_model) %>%
  mutate(
    model_name_ord = factor(
      model_name_full,
      levels = model_name_full
    )
  )

# forest plot
p6 <- ggplot2::ggplot(
  single_model_results_plot2,
  ggplot2::aes(
    x = model_name_ord,
    y = bias,
    ymin = lower_CI,
    ymax = upper_CI
  )
) +
  ggplot2::geom_pointrange(size = 0.4) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggplot2::geom_text(
    ggplot2::aes(label = bias_CI95),
    y = label_x,
    hjust = 0,
    size = 3
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::labs(
    title = "Forest Plot of Bias for Single Prediction Models",
    x = "Model",
    y = "Bias (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 9),
    plot.margin = ggplot2::margin(5.5, 90, 5.5, 5.5)
  ) +
  ggplot2::expand_limits(y = label_x)

# save tiff
ggplot2::ggsave(
  filename = file.path(getwd(), results_folder, "Figure 7.tiff"),
  plot = p6,
  device = "tiff",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)
