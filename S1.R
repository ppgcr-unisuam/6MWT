# ===============================
# Libraries
# ===============================
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(tibble)
library(officer)
library(ggplot2)

# ===============================
# Output dir
# ===============================
if (!dir.exists("results")) {
  dir.create("results")
}

# ===============================
# 1. Read file (preserve original column names)
# ===============================
df <- readxl::read_excel(
  "models.xlsx",
  col_names = FALSE
)

col_names <- df[1, ]
colnames(df) <- as.character(col_names)
df <- df[-1, ]  # remove header row

# ===============================
# 2. Identify equation columns + create stable keys
# ===============================
eq_cols_original <- colnames(df)[-c(1:5)]
equation_key <- sprintf("EQ%02d", seq_along(eq_cols_original))  # EQ001, EQ002...

equation_lookup <- tibble::tibble(
  key   = equation_key,
  label = eq_cols_original
)

# Rename equation columns in df to safe keys
colnames(df)[-c(1:5)] <- equation_key

# From now on, ALWAYS use keys as equation identifiers
equations <- equation_key

# Helper: key -> label
key_to_label <- setNames(equation_lookup$label, equation_lookup$key)

# ===============================
# 3. Variables to ignore
# ===============================
ignore_vars <- c("Year", "DOI", "Link", "Sample size", "Sex scope", "Sex code female", "Country", "Intercept")

# ===============================
# 4. Function: parameters used by each equation (keys)
# ===============================
params_used_in_equation <- function(eq_col_key) {
  df %>%
    dplyr::filter(!Variable %in% ignore_vars) %>%
    dplyr::filter(!is.na(.data[[eq_col_key]]) & .data[[eq_col_key]] != "") %>%
    dplyr::distinct(Variable) %>%
    dplyr::pull(Variable) %>%
    sort()
}

# ===============================
# 5. List of parameters per equation (keys)
# ===============================
param_sets <- purrr::map(equations, params_used_in_equation)
names(param_sets) <- equations

# ===============================
# 6. Summary table (grouped by variable set)
#    IMPORTANT: sort/group before mapping keys -> labels
# ===============================
summary_table_key <- tibble::tibble(
  EquationKey = equations,
  Variables   = purrr::map_chr(param_sets, ~ paste(.x, collapse = ", ")),
  Predictors  = purrr::map_int(param_sets, length)
) %>%
  dplyr::group_by(Variables, Predictors) %>%
  dplyr::summarise(
    Equations = dplyr::n(),
    Keys      = paste(EquationKey, collapse = ", "),
    .groups   = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Predictors), dplyr::desc(Equations))

# Convert "Keys" (comma-separated) -> "Authors" labels (comma-separated)
summary_table <- summary_table_key %>%
  dplyr::mutate(
    Authors = purrr::map_chr(
      strsplit(Keys, ",\\s*"),
      ~ paste(unname(key_to_label[.x]), collapse = ", ")
    )
  ) %>%
  dplyr::select(
    Authors,
    Equations,
    Predictors,
    Variables
  )

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
  print(target = file.path("results", "Table S1 - Summary of Parameter Sets.docx"))

# ===============================
# 8. Create binary parameter matrix (keys)
# ===============================
param_matrix <- tibble::tibble(
  Variable = sort(unique(unlist(param_sets)))
)

for (eq in equations) {
  param_matrix[[eq]] <- ifelse(
    param_matrix$Variable %in% param_sets[[eq]],
    1, 0
  )
}

# check Sex = 1 if Sex scope is used anywhere

# ===============================
# 9. Order equations by number of parameters (keys)
# ===============================
equation_sums <- colSums(as.data.frame(param_matrix[, -1, drop = FALSE]))
equations_sorted <- names(sort(equation_sums, decreasing = TRUE))

param_matrix_sorted <- param_matrix %>%
  dplyr::select(Variable, dplyr::all_of(equations_sorted))

# ===============================
# 9.5 Order variables by number of equations using them
# ===============================
variable_sums <- rowSums(
  as.data.frame(param_matrix_sorted[, -1, drop = FALSE])
)

param_matrix_sorted <- param_matrix_sorted %>%
  dplyr::mutate(n_equations = variable_sums) %>%
  dplyr::arrange(dplyr::desc(n_equations)) %>%
  dplyr::select(-n_equations)

# add model name (equation label) as first row
param_matrix_sorted <- rbind(
  c("Reference", equation_lookup$label[match(equations_sorted, equation_lookup$key)]),
  param_matrix_sorted
)

# collapse identical columns
param_matrix_sorted <- param_matrix_sorted[, !duplicated(as.list(param_matrix_sorted))]
# drop added row
param_matrix_sorted <- param_matrix_sorted[-1, ]
equation_lookup <- equation_lookup[!duplicated(equation_lookup$label), ]
equations_sorted <- colnames(param_matrix_sorted)[-1]

# ===============================
# 10. Long format for ggplot (no sanitization)
# ===============================
melted_param_matrix <- param_matrix_sorted %>%
  tidyr::pivot_longer(
    -Variable,
    names_to = "Equation",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    Equation = factor(Equation, levels = equations_sorted),
    Variable = factor(
      Variable,
      levels = rev(param_matrix_sorted$Variable)
    )
  )

# ===============================
# 11. Labels: number of parameters per equation (keys)
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
# 13. Plot heatmap (x-axis shows original labels)
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
  ggplot2::scale_x_discrete(
    labels = key_to_label[equations_sorted]
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
  file.path("results", "Figure 1.tiff"),
  plot = heatmap_plot,
  width = 10,
  height = 5,
  dpi = 300
)
