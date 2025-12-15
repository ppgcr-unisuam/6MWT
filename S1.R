# ===============================
# Libraries
# ===============================
library(dplyr)

# ===============================
# Output dir
# ===============================
if (!dir.exists("output")) {
  dir.create("output")
}

# ===============================
# 1. Read file
# ===============================
df <- readxl::read_excel("models.xlsx")

# ===============================
# 2. Identify equation columns
# ===============================
equations <- colnames(df)[-c(1:5)]

# ===============================
# 3. Variables to ignore
# ===============================
ignore_vars <- c("Year", "Country", "DOI", "Link", "Sample size", "Intercept")

# ===============================
# 4. Function: parameters used by each equation
# ===============================
params_used_in_equation <- function(eq_col) {
  df %>%
    dplyr::filter(!Variable %in% ignore_vars) %>%
    dplyr::filter(!is.na(.data[[eq_col]]) & .data[[eq_col]] != "") %>%
    dplyr::pull(Variable) %>%
    sort()
}

# ===============================
# 5. List of parameters per equation
# ===============================
param_sets <- purrr::map(equations, params_used_in_equation)

# ===============================
# 6. Summary table
# ===============================
summary_table <- tibble::tibble(
  Equation    = equations,
  Variables   = purrr::map_chr(param_sets, ~ paste(.x, collapse = ", ")),
  Predictors  = purrr::map_int(param_sets, length)
) %>%
  dplyr::group_by(Variables, Predictors) %>%
  dplyr::summarise(
    Equations = dplyr::n(),
    Authors   = paste(Equation, collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Predictors), dplyr::desc(Equations))

# reorder columns
summary_table <- summary_table %>%
  dplyr::select(Authors, Equations, Predictors, Variables)

# ===============================
# 7. Save Word table (landscape)
# ===============================
sec_properties <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape"),
  type = "continuous",
  page_margins = officer::page_mar(
    top = 2, bottom = 2,
    left = 2, right = 2
  )
)

officer::read_docx() %>%
  officer::body_add_par(
    "Table S1: Summary of Parameter Sets Used in Published Equations",
    style = "heading 1"
  ) %>%
  officer::body_add_table(
    summary_table,
    style = "table_template"
  ) %>%
  officer::body_end_section_landscape() %>%
  print(target = file.path("output", "Table S1 - Summary of Parameter Sets.docx"))

# ===============================
# 8. Create binary parameter matrix
# ===============================
param_matrix <- data.frame(
  Variable = unique(unlist(param_sets))
)

for (eq in equations) {
  param_matrix[[eq]] <- ifelse(
    param_matrix$Variable %in% params_used_in_equation(eq),
    1, 0
  )
}

# ===============================
# 9. Order equations by number of parameters
# ===============================
equation_sums <- colSums(param_matrix[, -1, drop = FALSE])
equations_sorted <- names(sort(equation_sums, decreasing = TRUE))

param_matrix_sorted <- param_matrix %>%
  dplyr::select(Variable, dplyr::all_of(equations_sorted))

# ===============================
# 10. Melt matrix for ggplot
# ===============================
melted_param_matrix <- reshape2::melt(
  param_matrix_sorted,
  id.vars = "Variable",
  variable.name = "Equation"
)

melted_param_matrix <- melted_param_matrix %>%
  dplyr::mutate(
    Equation = factor(Equation, levels = equations_sorted),
    Variable = factor(Variable, levels = rev(unique(Variable)))
  )

# ===============================
# 11. Labels: number of parameters per equation
# ===============================
label_df <- data.frame(
  Equation = factor(equations_sorted, levels = equations_sorted),
  y = length(unique(melted_param_matrix$Variable)) + 0.5,
  label = equation_sums[equations_sorted]
)

# ===============================
# 12. Vertical separator lines where parameter count changes
# ===============================
eq_df <- data.frame(
  Equation = equations_sorted,
  n_params = equation_sums[equations_sorted],
  pos = seq_along(equations_sorted)
)

vline_positions <- eq_df$pos[-1][diff(eq_df$n_params) != 0] - 0.5

# ===============================
# 13. Plot heatmap
# ===============================
heatmap_plot <- ggplot2::ggplot(
  melted_param_matrix,
  ggplot2::aes(x = Equation, y = Variable)
) +
  ggplot2::geom_tile(
    ggplot2::aes(fill = factor(value)),
    color = "white"
  ) +
  ggplot2::geom_vline(
    xintercept = vline_positions,
    color = "grey50",
    linewidth = 0.4
  ) +
  ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(x = Equation, y = y, label = label),
    inherit.aes = FALSE,
    size = 3,
    vjust = -0.2
  ) +
  ggplot2::scale_fill_manual(
    values = c("0" = "white", "1" = "grey")
  ) +
  ggplot2::scale_y_discrete(
    expand = ggplot2::expansion(add = c(0, 2))
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.title.y = ggplot2::element_text(face = "bold"),
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    panel.clip = "off",
    plot.margin = ggplot2::margin(10, 10, 20, 10)
  ) +
  ggplot2::labs(
    x = "Equations",
    y = "Predictors"
  ) +
  ggplot2::coord_fixed()

# ===============================
# 14. Save heatmap
# ===============================
ggplot2::ggsave(
  file.path("output", "Figure S1 - Parameter Usage Heatmap.tiff"),
  plot = heatmap_plot,
  width = 10,
  height = 5,
  dpi = 300
)
