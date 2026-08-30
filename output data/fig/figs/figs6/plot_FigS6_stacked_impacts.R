#!/usr/bin/env Rscript

# Figure S6 contract
# Core conclusion: current and optimized diets differ in both total environmental
# impact and food-group composition globally and across income, sex, and age strata.
# Evidence chain: (a) global totals; (b) income-group totals; (c) sex-specific
# totals aggregated across age; (d) age-specific totals aggregated across sex.
# Archetype: quantitative grid with a shared food-group visual vocabulary.

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
  normalizePath("plot_FigS6_stacked_impacts.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)
input_path <- file.path(script_dir, "Figs6.xlsx")
output_dir <- file.path(script_dir, "FigS6_output")

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
  # Intake is a daily absolute impact. Multiplying by 365 annualizes it;
  # the indicator-specific divisors then convert to the displayed units.
  Divisor = c(1e15, 1e13, 1e12, 1e12, 1e12)
)
metric_strip_labels <- setNames(
  metric_info$Metric,
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

assert_true(nrow(raw) == 37800L, "Expected 37,800 rows in Figs6.xlsx.")
assert_true(!anyNA(raw), "The workbook contains missing values.")
assert_true(!anyDuplicated(raw), "The workbook contains duplicated full rows.")
assert_true(all(raw$Intake >= 0), "Environmental impacts must be non-negative.")
assert_true(setequal(unique(raw$Scenario), scenario_levels), "Unexpected scenario labels.")
assert_true(setequal(unique(raw$Environment), environment_levels), "Unexpected environmental indicators.")
assert_true(
  setequal(unique(raw$FoodGroup), c(food_levels, "Total")),
  "Unexpected or missing food-group labels."
)

# Validate that Total is exactly the sum of the eight food-group components for
# every scenario-demographic-geography-indicator combination. Total is not used
# in the stacks, preventing double counting.
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

# a: World totals already supplied in the workbook.
panel_a_data <- plot_base %>%
  filter(region == "World", Age == "All", Sex == "All") %>%
  mutate(Group = "World")

# b: Four World Bank income groups; global and geographic-region rows are excluded.
panel_b_data <- plot_base %>%
  filter(region %in% income_levels, Age == "All", Sex == "All") %>%
  mutate(Group = factor(region, levels = income_levels, ordered = TRUE))

# c: Female and male totals are reconstructed by summing all mutually exclusive
# age bands. The Age == All / Sex == All rows are not mixed into this aggregation.
panel_c_data <- plot_base %>%
  filter(region == "World", Age != "All", Sex %in% c("FML", "MLE")) %>%
  group_by(Scenario, Sex, Environment, Metric, Unit, Divisor, FoodGroup) %>%
  summarise(Impact = sum(Impact), .groups = "drop") %>%
  mutate(
    Group = factor(
      recode(Sex, FML = "Female", MLE = "Male"),
      levels = c("Female", "Male"),
      ordered = TRUE
    ),
    Environment = factor(Environment, levels = environment_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_levels, ordered = TRUE)
  )

# d: Each age band is reconstructed by summing female and male values.
panel_d_data <- plot_base %>%
  filter(region == "World", Age != "All", Sex %in% c("FML", "MLE")) %>%
  group_by(Scenario, Age, Environment, Metric, Unit, Divisor, FoodGroup) %>%
  summarise(Impact = sum(Impact), .groups = "drop") %>%
  mutate(
    Group = factor(Age, levels = age_levels, ordered = TRUE),
    Environment = factor(Environment, levels = environment_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_levels, ordered = TRUE)
  )

# Independent aggregation checks: age-by-sex strata and income strata must both
# reproduce the supplied World/All/All rows.
world_reference <- plot_base %>%
  filter(region == "World", Age == "All", Sex == "All") %>%
  select(Scenario, Environment, FoodGroup, ReferenceImpact = Impact)
age_sex_sum <- plot_base %>%
  filter(region == "World", Age != "All", Sex %in% c("FML", "MLE")) %>%
  group_by(Scenario, Environment, FoodGroup) %>%
  summarise(StratumImpact = sum(Impact), .groups = "drop")
income_sum <- plot_base %>%
  filter(region %in% income_levels, Age == "All", Sex == "All") %>%
  group_by(Scenario, Environment, FoodGroup) %>%
  summarise(StratumImpact = sum(Impact), .groups = "drop")

reconciliation_error <- function(parts) {
  world_reference %>%
    left_join(parts, by = c("Scenario", "Environment", "FoodGroup")) %>%
    summarise(
      Error = max(
        abs(StratumImpact - ReferenceImpact) /
          pmax(abs(ReferenceImpact), .Machine$double.eps)
      )
    ) %>%
    pull(Error)
}
age_sex_error <- reconciliation_error(age_sex_sum)
income_error <- reconciliation_error(income_sum)
assert_true(age_sex_error < 1e-10, "Age-sex strata do not reconcile to World totals.")
assert_true(income_error < 1e-10, "Income strata do not reconcile to World totals.")

theme_figs6 <- function(base_size = 10.2) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      text = element_text(colour = "#111111"),
      axis.line = element_line(linewidth = 0.35, colour = "#111111"),
      axis.ticks = element_line(linewidth = 0.30, colour = "#111111"),
      axis.text = element_text(size = base_size, colour = "#111111"),
      axis.title = element_text(size = base_size + 0.3, colour = "#111111"),
      panel.grid.major.y = element_line(linewidth = 0.28, colour = "#D9D9D9"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#F2F2F2", colour = "#666666", linewidth = 0.35),
      strip.text = element_text(
        size = base_size + 0.2, face = "bold", lineheight = 0.92,
        margin = margin(t = 2.2, b = 2.2)
      ),
      panel.spacing.x = grid::unit(3.0, "mm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width = grid::unit(4.2, "mm"),
      plot.tag = element_text(size = base_size + 3.2, face = "bold"),
      plot.tag.position = "topleft",
      plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 4)),
      plot.margin = margin(4, 4, 3, 4)
    )
}

food_scale <- function() {
  scale_fill_manual(
    values = food_palette,
    limits = food_levels,
    drop = FALSE,
    guide = guide_legend(
      title = "Food group",
      nrow = 1,
      byrow = TRUE,
      title.position = "left"
    )
  )
}

format_bar_total <- function(x) {
  ifelse(
    x >= 100,
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE),
    format(round(x, 1), nsmall = 1, big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

prepare_paired_x <- function(data, group_levels, group_abbrev, full_scenario_labels = FALSE) {
  scenario_offset <- c("Current diets" = -0.22, "Optimized diets" = 0.22)
  scenario_abbrev <- c("Current diets" = "C", "Optimized diets" = "O")
  data %>%
    mutate(
      GroupCharacter = as.character(Group),
      GroupIndex = match(GroupCharacter, group_levels),
      ScenarioCharacter = as.character(Scenario),
      x = GroupIndex + unname(scenario_offset[ScenarioCharacter]),
      x_label = if (full_scenario_labels) {
        unname(scenario_abbrev[ScenarioCharacter])
      } else {
        unname(scenario_abbrev[ScenarioCharacter])
      }
    )
}

make_paired_panel <- function(
    data, group_levels, group_abbrev, tag,
    x_title = NULL, full_scenario_labels = FALSE,
    show_legend = TRUE, show_totals = TRUE, show_strip = TRUE,
    caption = NULL) {
  plot_data <- prepare_paired_x(data, group_levels, group_abbrev, full_scenario_labels)
  x_key <- plot_data %>%
    distinct(x, x_label) %>%
    arrange(x)
  group_label_data <- bind_rows(lapply(environment_levels, function(environment) {
    tibble(
      Environment = factor(environment, levels = environment_levels, ordered = TRUE),
      x = seq_along(group_levels),
      GroupLabel = unname(group_abbrev[group_levels])
    )
  }))
  totals <- plot_data %>%
    group_by(Environment, x) %>%
    summarise(Total = sum(Impact), .groups = "drop") %>%
    group_by(Environment) %>%
    mutate(
      Label = format_bar_total(Total),
      # Put the tallest label inside its bar so it cannot be clipped by the strip.
      LabelHjust = if_else(Total >= 0.75 * max(Total), 1.05, -0.08)
    ) %>%
    ungroup()

  p <- ggplot(plot_data, aes(x = x, y = Impact, fill = FoodGroup)) +
    geom_col(width = 0.38, colour = NA) +
    facet_wrap(
      vars(Environment), nrow = 1, scales = "free_y",
      labeller = as_labeller(metric_strip_labels)
    ) +
    food_scale() +
    scale_x_continuous(
      breaks = x_key$x,
      labels = x_key$x_label,
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ",", accuracy = 0.1),
      expand = expansion(mult = c(0, if (show_totals) 0.18 else 0.06))
    ) +
    labs(
      x = x_title,
      y = "Environmental impact",
      tag = tag,
      caption = caption
    ) +
    theme_figs6() +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x = element_text(size = 10, lineheight = 0.86),
      axis.title.x = element_text(size = 10.5, margin = margin(t = 9)),
      plot.tag.position = "topleft",
      strip.background = if (show_strip) {
        element_rect(fill = "#F2F2F2", colour = "#666666", linewidth = 0.35)
      } else {
        element_blank()
      },
      strip.text = if (show_strip) {
        element_text(
          size = 10.5, face = "bold",
          margin = margin(t = 2.2, b = 2.2)
        )
      } else {
        element_blank()
      }
    )

  if (!full_scenario_labels) {
    p <- p +
      geom_text(
        data = group_label_data,
        aes(x = x, y = 0, label = GroupLabel),
        inherit.aes = FALSE,
        family = "Arial", size = 10 / ggplot2::.pt,
        vjust = 3.0, colour = "#111111"
      ) +
      coord_cartesian(clip = "off")
  }

  if (show_totals) {
    p <- p + geom_text(
      data = totals,
      aes(x = x, y = Total, label = Label, hjust = LabelHjust),
      inherit.aes = FALSE,
      angle = 90,
      vjust = 0.5,
      size = 10 / ggplot2::.pt,
      family = "Arial",
      fontface = "bold",
      colour = "#111111"
    )
  }
  p
}

make_panel_a <- function(
    show_legend = TRUE, show_strip = TRUE,
    caption = "C, current; O, optimized.") {
  make_paired_panel(
    panel_a_data,
    group_levels = "World",
    group_abbrev = c("World" = "W"),
    tag = "a",
    x_title = "Scenario",
    full_scenario_labels = TRUE,
    show_legend = show_legend,
    show_totals = FALSE,
    show_strip = show_strip,
    caption = caption
  )
}

make_panel_b <- function(show_legend = TRUE, show_strip = TRUE) {
  make_paired_panel(
    panel_b_data,
    group_levels = income_levels,
    group_abbrev = c(
      "High income" = "H",
      "Upper-middle income" = "UM",
      "Lower middle income" = "LM",
      "Low income" = "L"
    ),
    tag = "b",
    x_title = "Income group × scenario",
    show_legend = show_legend,
    show_totals = FALSE,
    show_strip = show_strip,
    caption = "H, high; UM, upper-middle; LM, lower-middle; L, low; C, current; O, optimized."
  )
}

make_panel_c <- function(show_legend = TRUE, show_strip = TRUE) {
  make_paired_panel(
    panel_c_data,
    group_levels = c("Female", "Male"),
    group_abbrev = c("Female" = "F", "Male" = "M"),
    tag = "c",
    x_title = "Sex × scenario",
    show_legend = show_legend,
    show_totals = FALSE,
    show_strip = show_strip,
    caption = "F, female; M, male; C, current; O, optimized."
  )
}

metric_ymax <- panel_d_data %>%
  group_by(Environment, Scenario, Group) %>%
  summarise(Total = sum(Impact), .groups = "drop") %>%
  group_by(Environment) %>%
  summarise(Maximum = max(Total), .groups = "drop")

make_age_metric_plot <- function(
    environment, scenario, show_x = TRUE,
    show_x_title = FALSE, show_y_title = FALSE,
    show_strip = TRUE, show_scenario_label = FALSE,
    show_legend = TRUE, tag = NULL) {
  plot_data <- panel_d_data %>%
    filter(Environment == environment, Scenario == scenario)
  ymax <- metric_ymax %>%
    filter(Environment == environment) %>%
    pull(Maximum)

  ggplot(plot_data, aes(x = Group, y = Impact, fill = FoodGroup)) +
    geom_col(width = 0.76, colour = NA) +
    facet_wrap(
      vars(Environment), nrow = 1,
      labeller = as_labeller(metric_strip_labels)
    ) +
    food_scale() +
    scale_x_discrete(
      breaks = age_display_breaks,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, ymax * 1.06),
      labels = label_number(big.mark = ",", accuracy = 0.1),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = if (show_x_title) "Age group (years)" else NULL,
      y = if (show_y_title) "Environmental impact" else NULL,
      subtitle = if (show_scenario_label) as.character(scenario) else NULL,
      tag = tag
    ) +
    theme_figs6(base_size = 10) +
    theme(
      axis.text.x = if (show_x) {
        element_text(angle = 45, hjust = 1, vjust = 1, size = 10)
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x) element_line(linewidth = 0.3) else element_blank(),
      axis.title.x = if (show_x_title) element_text(size = 10) else element_blank(),
      axis.title.y = if (show_y_title) element_text(size = 10) else element_blank(),
      plot.subtitle = if (show_scenario_label) {
        element_text(size = 10, face = "italic", margin = margin(b = 1))
      } else {
        element_blank()
      },
      strip.background = if (show_strip) {
        element_rect(fill = "#F2F2F2", colour = "#666666", linewidth = 0.35)
      } else {
        element_blank()
      },
      strip.text = if (show_strip) {
        element_text(
          size = 10.5, face = "bold",
          margin = margin(t = 2.0, b = 2.0)
        )
      } else {
        element_blank()
      },
      legend.position = if (show_legend) "bottom" else "none",
      plot.tag = element_text(size = 13, face = "bold"),
      plot.tag.position = "topleft",
      plot.margin = margin(3, 2, 2, if (show_y_title) 6 else 2)
    )
}

make_food_key <- function() {
  key_data <- tibble(
    x = seq_along(food_levels),
    FoodGroup = factor(food_levels, levels = food_levels, ordered = TRUE),
    Label = food_levels
  )

  ggplot(key_data, aes(x = x, y = 0, fill = FoodGroup)) +
    geom_tile(width = 0.34, height = 0.26) +
    geom_text(
      aes(x = x, y = -0.22, label = Label),
      inherit.aes = FALSE,
      family = "Arial", size = 10 / ggplot2::.pt,
      vjust = 1, colour = "#111111"
    ) +
    annotate(
      "text", x = 0.52, y = 0.25, label = "Food group",
      hjust = 0, family = "Arial", fontface = "bold",
      size = 10 / ggplot2::.pt, colour = "#111111"
    ) +
    scale_fill_manual(values = food_palette, limits = food_levels, drop = FALSE) +
    coord_cartesian(xlim = c(0.5, length(food_levels) + 0.5), ylim = c(-0.62, 0.34), expand = FALSE) +
    theme_void(base_family = "Arial") +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 4, 0, 4)
    )
}

make_panel_d <- function(
    show_legend = TRUE, include_tag = TRUE,
    show_strip = TRUE, show_y_title = TRUE) {
  current_row <- wrap_plots(
    lapply(seq_along(environment_levels), function(i) {
      make_age_metric_plot(
        environment_levels[[i]], "Current diets",
        show_x = FALSE,
        show_x_title = FALSE,
        show_y_title = show_y_title && i == 1L,
        show_strip = show_strip,
        show_scenario_label = i == 1L,
        show_legend = FALSE,
        tag = if (include_tag && i == 1L) "d" else NULL
      )
    }),
    nrow = 1,
    guides = "collect"
  )
  optimized_row <- wrap_plots(
    lapply(seq_along(environment_levels), function(i) {
      make_age_metric_plot(
        environment_levels[[i]], "Optimized diets",
        show_x = TRUE,
        show_x_title = i == 3L,
        show_y_title = FALSE,
        show_strip = FALSE,
        show_scenario_label = i == 1L,
        show_legend = FALSE
      )
    }),
    nrow = 1,
    guides = "collect"
  )
  if (show_legend) {
    legend_row <- make_food_key()
    out <- current_row / optimized_row / legend_row +
      plot_layout(heights = c(0.82, 1.02, 0.32), guides = "keep")
  } else {
    out <- current_row / optimized_row +
      plot_layout(heights = c(0.86, 1.14), guides = "keep")
  }
  out
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

panel_a <- make_panel_a(show_legend = TRUE, show_strip = TRUE)
panel_b <- make_panel_b(show_legend = TRUE, show_strip = TRUE)
panel_c <- make_panel_c(show_legend = TRUE, show_strip = TRUE)
panel_d <- make_panel_d(show_legend = TRUE, include_tag = TRUE, show_strip = TRUE)

save_plot_bundle(panel_a, file.path(output_dir, "FigS6a_global"), 230, 62)
save_plot_bundle(panel_b, file.path(output_dir, "FigS6b_income_groups"), 230, 80)
save_plot_bundle(panel_c, file.path(output_dir, "FigS6c_sex"), 230, 72)
save_plot_bundle(panel_d, file.path(output_dir, "FigS6d_age_groups"), 230, 125)

combined <- (
  make_panel_a(
    show_legend = FALSE, show_strip = TRUE,
    caption = NULL
  ) /
    make_panel_b(show_legend = FALSE, show_strip = FALSE) /
    make_panel_c(show_legend = FALSE, show_strip = FALSE) /
    make_panel_d(
      show_legend = TRUE, include_tag = TRUE,
      show_strip = FALSE, show_y_title = FALSE
    )
) +
  plot_layout(heights = c(0.76, 0.94, 0.86, 1.72), guides = "keep")

save_plot_bundle(combined, file.path(output_dir, "FigS6_combined"), 230, 280)

source_data <- bind_rows(
  panel_a_data %>%
    transmute(
      Panel = "S6a", Scenario = as.character(Scenario),
      GroupType = "Global", Group = "World",
      Environment = as.character(Environment), Metric, Unit,
      FoodGroup = as.character(FoodGroup), Impact
    ),
  panel_b_data %>%
    transmute(
      Panel = "S6b", Scenario = as.character(Scenario),
      GroupType = "Income group", Group = as.character(Group),
      Environment = as.character(Environment), Metric, Unit,
      FoodGroup = as.character(FoodGroup), Impact
    ),
  panel_c_data %>%
    transmute(
      Panel = "S6c", Scenario = as.character(Scenario),
      GroupType = "Sex", Group = as.character(Group),
      Environment = as.character(Environment), Metric, Unit,
      FoodGroup = as.character(FoodGroup), Impact
    ),
  panel_d_data %>%
    transmute(
      Panel = "S6d", Scenario = as.character(Scenario),
      GroupType = "Age group", Group = as.character(Group),
      Environment = as.character(Environment), Metric, Unit,
      FoodGroup = as.character(FoodGroup), Impact
    )
)
write.csv(
  source_data,
  file.path(output_dir, "FigS6_source_data.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

qa_lines <- c(
  "Figure S6 QA summary",
  paste0("Input rows: ", format(nrow(raw), big.mark = ",")),
  paste0("Missing cells: ", sum(is.na(raw))),
  paste0("Duplicated full rows: ", sum(duplicated(raw))),
  paste0("Negative impact values: ", sum(raw$Intake < 0)),
  paste0("Maximum component-vs-Total relative difference: ", format(max(component_check$RelativeDifference), scientific = TRUE)),
  paste0("Maximum age-sex-vs-World relative difference: ", format(age_sex_error, scientific = TRUE)),
  paste0("Maximum income-vs-World relative difference: ", format(income_error, scientific = TRUE)),
  "Excluded from stacks: FoodGroup == Total only (used for reconciliation).",
  "Panel c aggregation: summed 17 mutually exclusive age bands within female or male.",
  "Panel d aggregation: summed female and male within each of 17 age bands.",
  "Unit conversion: daily Intake multiplied by 365, then divided by the indicator-specific divisor.",
  "Visual annotation: bar-total labels omitted; values remain available in source data.",
  "Typography: minimum configured visible text size is 10 pt.",
  "Dimensions: combined figure 230 x 280 mm; all standalone figures are <=230 mm wide and <=125 mm high.",
  "Strip policy: indicator strips appear once at the top of the combined figure and contain no units.",
  "Exports: editable SVG, vector PDF, 600-dpi LZW TIFF, and 300-dpi PNG."
)
writeLines(qa_lines, file.path(output_dir, "FigS6_QA.txt"), useBytes = TRUE)

message("Figure S6 exports written to: ", output_dir)
message("Age-sex reconciliation error: ", format(age_sex_error, scientific = TRUE))
message("Income reconciliation error: ", format(income_error, scientific = TRUE))
