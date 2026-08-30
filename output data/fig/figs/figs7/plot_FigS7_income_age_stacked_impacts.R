#!/usr/bin/env Rscript

# Figure S7 contract
# Core conclusion: age-specific environmental burdens and their food-group
# composition differ across income groups under both current and optimized diets.
# Evidence chain: panels a-e separate the five environmental indicators; within
# each panel, rows show income groups and columns separate the two scenarios.
# Archetype: quantitative grid with a shared food-group visual vocabulary.
# Scale policy: each income group has its own y-axis maximum, shared by the two
# scenarios within that income group so current-vs-optimized comparisons remain valid.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

required_packages <- c("svglite", "ragg")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- command_args[grepl("^--file=", command_args)]
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("plot_FigS7_income_age_stacked_impacts.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)
input_path <- file.path(script_dir, "Figs7.xlsx")
output_dir <- file.path(script_dir, "FigS7_output")

if (!file.exists(input_path)) {
  stop("Missing input workbook: ", input_path, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

environment_levels <- c("GHG", "Land", "Freshwater", "Eutr.", "Acid.")
scenario_levels <- c("Current diets", "Optimized diets")
food_levels <- c(
  "Seafood", "Dairy & eggs", "Meat", "Sugar & oil",
  "Staple foods", "Legumes & nuts", "Fruits", "Vegetable"
)
income_levels <- c(
  "High income", "Upper-middle income",
  "Lower middle income", "Low income"
)
age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)
age_display_breaks <- c("0-4", "20-24", "40-44", "60-64", "80+")

metric_info <- tibble(
  Environment = environment_levels,
  Metric = c(
    "GHG emissions", "Land use", "Water use",
    "Eutrophication", "Acidification"
  ),
  Unit = c("Gt CO₂-eq", "Mha", "km³", "Mt PO₄³⁻-eq", "Mt SO₂-eq"),
  Divisor = c(1e15, 1e13, 1e12, 1e12, 1e12)
)
metric_strip_labels <- setNames(
  paste0(metric_info$Metric, "\n(", metric_info$Unit, ")"),
  metric_info$Environment
)

food_palette <- c(
  "Seafood" = "#5E93B5",
  "Dairy & eggs" = "#AE92A9",
  "Meat" = "#B55A49",
  "Sugar & oil" = "#C2827E",
  "Staple foods" = "#C89A4A",
  "Legumes & nuts" = "#2E8977",
  "Fruits" = "#B8D4A6",
  "Vegetable" = "#8FB5A2"
)

raw <- read_excel(input_path, sheet = 1)
required_columns <- c(
  "Scenario", "Age", "Sex", "Environment", "FoodGroup", "region", "Intake"
)
missing_columns <- setdiff(required_columns, names(raw))
assert_true(
  length(missing_columns) == 0L,
  paste0("Workbook is missing required column(s): ", paste(missing_columns, collapse = ", "))
)

raw <- raw %>%
  transmute(
    Scenario = as.character(Scenario),
    Age = as.character(Age),
    Sex = as.character(Sex),
    Environment = as.character(Environment),
    FoodGroup = as.character(FoodGroup),
    region = as.character(region),
    Intake = as.numeric(Intake)
  )

assert_true(nrow(raw) == 37800L, "Expected 37,800 rows in Figs7.xlsx.")
assert_true(!anyNA(raw), "The workbook contains missing values.")
assert_true(!anyDuplicated(raw), "The workbook contains duplicated full rows.")
assert_true(all(raw$Intake >= 0), "Environmental impacts must be non-negative.")
assert_true(setequal(unique(raw$Scenario), scenario_levels), "Unexpected scenario labels.")
assert_true(setequal(unique(raw$Environment), environment_levels), "Unexpected environmental indicators.")
assert_true(
  setequal(unique(raw$FoodGroup), c(food_levels, "Total")),
  "Unexpected or missing food-group labels."
)
assert_true(
  all(income_levels %in% unique(raw$region)),
  "One or more expected income groups are missing."
)
assert_true(
  all(age_levels %in% unique(raw$Age)),
  "One or more expected age groups are missing."
)

# Validate the reported Total against the eight stacked components for every
# scenario-demographic-geography-indicator combination. Total is retained only
# for integrity checks and is never included in a stack.
component_check <- raw %>%
  group_by(Scenario, Age, Sex, Environment, region) %>%
  summarise(
    ComponentSum = sum(Intake[FoodGroup != "Total"]),
    ReportedTotal = Intake[FoodGroup == "Total"],
    TotalRows = sum(FoodGroup == "Total"),
    ComponentRows = sum(FoodGroup != "Total"),
    .groups = "drop"
  ) %>%
  mutate(
    RelativeDifference = abs(ComponentSum - ReportedTotal) /
      pmax(abs(ReportedTotal), .Machine$double.eps)
  )
assert_true(
  all(component_check$TotalRows == 1L & component_check$ComponentRows == 8L),
  "Each stack must contain eight components and one Total row."
)
assert_true(
  max(component_check$RelativeDifference) < 1e-10,
  "At least one Total row does not equal the sum of its food-group components."
)

plot_base <- raw %>%
  filter(FoodGroup != "Total") %>%
  left_join(metric_info, by = "Environment") %>%
  mutate(
    Impact = Intake * 365 / Divisor,
    Environment = factor(Environment, levels = environment_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_levels, ordered = TRUE)
  )

# Reconstruct each age band by summing the mutually exclusive female and male
# values separately within each income group.
panel_data <- plot_base %>%
  filter(
    region %in% income_levels,
    Age %in% age_levels,
    Sex %in% c("FML", "MLE")
  ) %>%
  group_by(region, Scenario, Age, Environment, Metric, Unit, Divisor, FoodGroup) %>%
  summarise(Impact = sum(Impact), .groups = "drop") %>%
  mutate(
    Income = factor(region, levels = income_levels, ordered = TRUE),
    Age = factor(Age, levels = age_levels, ordered = TRUE),
    Environment = factor(Environment, levels = environment_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_levels, ordered = TRUE)
  ) %>%
  select(-region)

expected_panel_rows <- length(income_levels) * length(scenario_levels) *
  length(age_levels) * length(environment_levels) * length(food_levels)
assert_true(
  nrow(panel_data) == expected_panel_rows,
  paste0("Expected ", expected_panel_rows, " plotted component rows.")
)

# For every income group, the 17 age bands (with both sexes summed within each
# band) must reproduce the supplied Age == All / Sex == All income total.
income_reference <- plot_base %>%
  filter(region %in% income_levels, Age == "All", Sex == "All") %>%
  transmute(
    Income = factor(region, levels = income_levels, ordered = TRUE),
    Scenario, Environment, FoodGroup, ReferenceImpact = Impact
  )
income_age_sum <- panel_data %>%
  group_by(Income, Scenario, Environment, FoodGroup) %>%
  summarise(StratumImpact = sum(Impact), .groups = "drop")
income_reconciliation <- income_reference %>%
  left_join(
    income_age_sum,
    by = c("Income", "Scenario", "Environment", "FoodGroup")
  ) %>%
  mutate(
    RelativeDifference = abs(StratumImpact - ReferenceImpact) /
      pmax(abs(ReferenceImpact), .Machine$double.eps)
  )
income_age_error <- max(income_reconciliation$RelativeDifference)
assert_true(
  income_age_error < 1e-10,
  "Income-specific age strata do not reconcile to the supplied income totals."
)

# Maxima are calculated separately for each indicator-income combination and
# shared by the two scenarios, prioritizing age-pattern visibility within income.
metric_ymax <- panel_data %>%
  group_by(Environment, Income, Scenario, Age) %>%
  summarise(Total = sum(Impact), .groups = "drop") %>%
  group_by(Environment, Income) %>%
  summarise(Maximum = max(Total), .groups = "drop")

theme_figs7 <- function(base_size = 9.2) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = "#111111"),
      axis.line = element_line(linewidth = 0.34, colour = "#111111"),
      axis.ticks = element_line(linewidth = 0.28, colour = "#111111"),
      axis.text = element_text(size = base_size, colour = "#111111"),
      axis.title = element_text(size = base_size + 0.2, colour = "#111111"),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "#D9D9D9"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(
        fill = "#F2F2F2", colour = "#666666", linewidth = 0.34
      ),
      strip.text = element_text(
        size = base_size + 0.1, face = "bold", lineheight = 0.92,
        margin = margin(t = 2.1, b = 2.1)
      ),
      panel.spacing.x = grid::unit(2.4, "mm"),
      legend.position = "none",
      plot.tag = element_text(size = base_size + 3.4, face = "bold"),
      plot.tag.position = "topleft",
      plot.subtitle = element_text(size = base_size + 0.1, face = "bold"),
      plot.margin = margin(2.5, 1.5, 1.5, 1.5)
    )
}

food_scale <- function() {
  scale_fill_manual(values = food_palette, limits = food_levels, drop = FALSE)
}

make_income_scenario_plot <- function(
    income, environment, scenario,
    show_x = TRUE, show_x_title = FALSE,
    show_y_title = FALSE, show_y_axis = TRUE,
    show_scenario_heading = FALSE,
    show_income_label = FALSE) {
  plot_data <- panel_data %>%
    filter(
      Income == income,
      Environment == environment,
      Scenario == scenario
    )
  ymax <- metric_ymax %>%
    filter(Environment == environment, Income == income) %>%
    pull(Maximum)

  ggplot(plot_data, aes(x = Age, y = Impact, fill = FoodGroup)) +
    geom_col(width = 0.76, colour = NA) +
    food_scale() +
    scale_x_discrete(breaks = age_display_breaks, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, ymax * 1.04),
      labels = label_number(big.mark = ",", accuracy = 0.1),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = if (show_x_title) "Age group (years)" else NULL,
      y = if (show_y_title) "Annual environmental impact" else NULL,
      title = if (show_scenario_heading) as.character(scenario) else NULL,
      subtitle = if (show_income_label) as.character(income) else NULL
    ) +
    theme_figs7() +
    theme(
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, vjust = 1, size = 8.7)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x) element_line(linewidth = 0.28) else element_blank(),
      axis.title.x = if (show_x_title) element_text(size = 9.2) else element_blank(),
      axis.title.y = if (show_y_title) element_text(size = 9.2) else element_blank(),
      axis.text.y = if (show_y_axis) element_text(size = 8.7) else element_blank(),
      axis.ticks.y = if (show_y_axis) element_line(linewidth = 0.28) else element_blank(),
      axis.line.y = if (show_y_axis) {
        element_line(linewidth = 0.34, colour = "#111111")
      } else {
        element_blank()
      },
      plot.title = if (show_scenario_heading) {
        element_text(
          size = 9.6, face = "bold", hjust = 0.5,
          margin = margin(b = 1.5)
        )
      } else {
        element_blank()
      },
      plot.subtitle = if (show_income_label) {
        element_text(
          size = 9.2, face = "bold", hjust = 0,
          margin = margin(b = 1.2)
        )
      } else {
        element_blank()
      },
      plot.margin = margin(2.4, 1.5, 1.5, if (show_y_title) 5.5 else 1.5)
    )
}

make_food_key <- function() {
  key_data <- tibble(
    x = seq_along(food_levels),
    FoodGroup = factor(food_levels, levels = food_levels, ordered = TRUE),
    Label = food_levels
  )

  ggplot(key_data, aes(x = x, y = 0, fill = FoodGroup)) +
    geom_tile(width = 0.34, height = 0.25) +
    geom_text(
      aes(x = x, y = -0.21, label = Label),
      inherit.aes = FALSE,
      family = "Arial", size = 9.2 / ggplot2::.pt,
      vjust = 1, colour = "#111111"
    ) +
    annotate(
      "text", x = 0.52, y = 0.25, label = "Food group",
      hjust = 0, family = "Arial", fontface = "bold",
      size = 9.2 / ggplot2::.pt, colour = "#111111"
    ) +
    scale_fill_manual(values = food_palette, limits = food_levels, drop = FALSE) +
    coord_cartesian(
      xlim = c(0.5, length(food_levels) + 0.5),
      ylim = c(-0.60, 0.34), expand = FALSE
    ) +
    theme_void(base_family = "Arial") +
    theme(legend.position = "none", plot.margin = margin(0, 4, 0, 4))
}

environment_tags <- setNames(c("a", "b", "c", "d", "e"), environment_levels)

make_environment_figure <- function(environment) {
  plot_list <- unlist(
    lapply(seq_along(income_levels), function(income_index) {
      income <- income_levels[[income_index]]
      lapply(seq_along(scenario_levels), function(scenario_index) {
        scenario <- scenario_levels[[scenario_index]]
        make_income_scenario_plot(
          income = income,
          environment = environment,
          scenario = scenario,
          show_x = income_index == length(income_levels),
          show_x_title = income_index == length(income_levels),
          show_y_title = income_index == 2L && scenario_index == 1L,
          show_y_axis = scenario_index == 1L,
          show_scenario_heading = income_index == 1L,
          show_income_label = scenario_index == 1L
        )
      })
    }),
    recursive = FALSE
  )

  metric_row <- metric_info %>% filter(Environment == environment)
  figure_title <- paste0(
    unname(environment_tags[environment]), "  ",
    metric_row$Metric, " (", metric_row$Unit, ")"
  )

  header <- ggplot() +
    annotate(
      "text", x = 0.01, y = 0.5, label = figure_title,
      hjust = 0, vjust = 0.5, family = "Arial", fontface = "bold",
      size = 11.2 / ggplot2::.pt, colour = "#111111"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void(base_family = "Arial") +
    theme(plot.margin = margin(0, 0, 0, 0))

  plot_grid <- wrap_plots(plot_list, ncol = 2, byrow = TRUE)
  header / plot_grid / make_food_key() +
    plot_layout(heights = c(0.055, 1, 0.13))
}

save_plot_bundle <- function(plot, stem, width_mm, height_mm, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  svglite::svglite(
    paste0(stem, ".svg"),
    width = width_in, height = height_in,
    bg = "white", system_fonts = list(sans = "Arial")
  )
  print(plot)
  grDevices::dev.off()

  grDevices::cairo_pdf(
    paste0(stem, ".pdf"),
    width = width_in, height = height_in,
    family = "Arial", bg = "white"
  )
  print(plot)
  grDevices::dev.off()

  ragg::agg_tiff(
    paste0(stem, ".tiff"),
    width = width_in, height = height_in,
    units = "in", res = dpi, background = "white",
    compression = "lzw"
  )
  print(plot)
  grDevices::dev.off()

  ragg::agg_png(
    paste0(stem, ".png"),
    width = width_in, height = height_in,
    units = "in", res = 300, background = "white"
  )
  print(plot)
  grDevices::dev.off()
}

environment_stems <- c(
  "GHG" = "FigS7a_GHG_emissions",
  "Land" = "FigS7b_land_use",
  "Freshwater" = "FigS7c_freshwater_use",
  "Eutr." = "FigS7d_eutrophication",
  "Acid." = "FigS7e_acidification"
)

for (environment in environment_levels) {
  panel <- make_environment_figure(environment)
  save_plot_bundle(
    panel,
    file.path(output_dir, unname(environment_stems[environment])),
    width_mm = 230,
    height_mm = 190
  )
}

source_data <- panel_data %>%
  transmute(
    Panel = unname(environment_tags[as.character(Environment)]),
    Income = as.character(Income),
    Scenario = as.character(Scenario),
    Age = as.character(Age),
    Environment = as.character(Environment),
    Metric,
    Unit,
    FoodGroup = as.character(FoodGroup),
    Impact
  )
write.csv(
  source_data,
  file.path(output_dir, "FigS7_source_data.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

qa_lines <- c(
  "Figure S7 QA summary",
  paste0("Input rows: ", format(nrow(raw), big.mark = ",")),
  paste0("Plotted component rows: ", format(nrow(panel_data), big.mark = ",")),
  paste0("Missing cells: ", sum(is.na(raw))),
  paste0("Duplicated full rows: ", sum(duplicated(raw))),
  paste0("Negative impact values: ", sum(raw$Intake < 0)),
  paste0(
    "Maximum component-vs-Total relative difference: ",
    format(max(component_check$RelativeDifference), scientific = TRUE)
  ),
  paste0(
    "Maximum income-age-vs-income-total relative difference: ",
    format(income_age_error, scientific = TRUE)
  ),
  "Excluded from stacks: FoodGroup == Total only (used for reconciliation).",
  "Aggregation: female and male were summed within each of 17 mutually exclusive age bands and each income group.",
  "Unit conversion: daily Intake multiplied by 365, then divided by the indicator-specific divisor.",
  "Scale policy: each income group has an independent y-axis maximum, shared by current and optimized scenarios within that income group.",
  "Scale caveat: bar heights must not be compared directly across income-group rows because their y-axis limits differ.",
  "Interpretation caveat: values are aggregate impacts and therefore reflect both age structure/population size and diet composition; no per-capita denominator was supplied.",
  "Typography: minimum configured visible text size is 8.7 pt.",
  "Layout: five separate figures, one for each environmental indicator; income groups are rows and scenarios are columns.",
  "Dimensions: each environmental-indicator figure is 230 x 190 mm.",
  "Exports: editable SVG, vector PDF, 600-dpi LZW TIFF, and 300-dpi PNG."
)
writeLines(qa_lines, file.path(output_dir, "FigS7_QA.txt"), useBytes = TRUE)

message("Figure S7 exports written to: ", output_dir)
message("Income-age reconciliation error: ", format(income_age_error, scientific = TRUE))
