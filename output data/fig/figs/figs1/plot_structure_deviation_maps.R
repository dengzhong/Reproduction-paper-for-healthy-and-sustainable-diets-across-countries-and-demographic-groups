#!/usr/bin/env Rscript

# Age-specific dietary structure deviation maps by sex.
# Metric for each country × sex × age group:
#   mean(abs(optimized - current) / current) across non-Total food groups.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

required_packages <- c("ragg", "svglite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running this script."
  )
}

# Resolve files relative to this script, so the script can be run from any folder.
command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- command_args[grepl("^--file=", command_args)]
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("plot_structure_deviation_maps.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)

input_xlsx <- file.path(script_dir, "Intake_groups_all_demographic.xlsx")
world_geojson <- file.path(dirname(script_dir), "world.geojson")
output_dir <- file.path(script_dir, "structure_deviation_maps")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)
analysis_age_levels <- c(age_levels, "PerCapita")
age_display_labels <- setNames(
  gsub("-", "\u2013", age_levels, fixed = TRUE),
  age_levels
)

required_columns <- c("Scenario", "ISO3", "Age", "Sex", "FoodGroup", "Intake")
intake <- read_excel(input_xlsx)
missing_columns <- setdiff(required_columns, names(intake))
if (length(missing_columns) > 0L) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = ", "))
}

expected_scenarios <- c("Current diets", "Optimized diets")
unexpected_scenarios <- setdiff(unique(intake$Scenario), expected_scenarios)
missing_scenarios <- setdiff(expected_scenarios, unique(intake$Scenario))
if (length(unexpected_scenarios) > 0L || length(missing_scenarios) > 0L) {
  stop(
    "Scenario labels do not match the expected Current/Optimized pair. ",
    "Unexpected: ", paste(unexpected_scenarios, collapse = ", "),
    "; missing: ", paste(missing_scenarios, collapse = ", ")
  )
}

duplicate_keys <- intake |>
  count(Scenario, ISO3, Age, Sex, FoodGroup, name = "n") |>
  filter(n > 1L)
if (nrow(duplicate_keys) > 0L) {
  stop("Duplicate Scenario × ISO3 × Age × Sex × FoodGroup records were found.")
}

# FoodGroup == Total is explicitly excluded from both the numerator and the
# food-group count. No imputation or weighting is applied.
paired <- intake |>
  filter(FoodGroup != "Total", Age %in% analysis_age_levels) |>
  select(Scenario, ISO3, Age, Sex, FoodGroup, Intake) |>
  pivot_wider(names_from = Scenario, values_from = Intake)

if (anyNA(paired$`Current diets`) || anyNA(paired$`Optimized diets`)) {
  stop("At least one food group lacks a matched Current or Optimized value.")
}
if (any(paired$`Current diets` <= 0)) {
  stop(
    "The relative-change metric is undefined because at least one Current ",
    "diet intake is zero or negative. No rows were silently removed."
  )
}

scores <- paired |>
  mutate(
    relative_change = abs(`Optimized diets` - `Current diets`) / `Current diets`
  ) |>
  group_by(ISO3, Sex, Age) |>
  summarise(
    deviation = mean(relative_change),
    deviation_percent = 100 * deviation,
    n_food_groups = n(),
    .groups = "drop"
  ) |>
  mutate(
    Sex = recode(Sex, MLE = "Male", FML = "Female"),
    Age = factor(Age, levels = analysis_age_levels, ordered = TRUE)
  )

if (any(scores$n_food_groups != 8L)) {
  stop("Expected exactly 8 non-Total food groups for every country/sex/age cell.")
}

previous_s2_setting <- sf_use_s2()
sf_use_s2(FALSE)
world <- st_read(world_geojson, quiet = TRUE) |>
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) |>
  mutate(
    ISO3 = as.character(SOC),
    # The supplied map uses two legacy ISO3 codes.
    ISO3 = recode(ISO3, ROM = "ROU", TMP = "TLS")
  ) |>
  st_make_valid() |>
  # Split only geometries crossing ±180° before the Mollweide transform. This
  # prevents Russia from being closed across the full map width.
  suppressWarnings(st_break_antimeridian(lon_0 = 0, tol = 1e-4))
sf_use_s2(previous_s2_setting)

unmatched_iso3 <- setdiff(unique(scores$ISO3), unique(world$ISO3))
if (length(unmatched_iso3) > 0L) {
  stop(
    "Country code(s) in the intake data are absent from the map after known ",
    "legacy-code harmonization: ", paste(unmatched_iso3, collapse = ", ")
  )
}

# Replicate the map once per age facet, then attach each sex-specific score.
world_by_age <- world[rep(seq_len(nrow(world)), times = length(age_levels)), ]
world_by_age$Age <- rep(age_levels, each = nrow(world))

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

score_range <- range(scores$deviation_percent, finite = TRUE)
colour_max <- 400
colour_limits <- c(score_range[1], colour_max)
legend_breaks <- c(50, 100, 200, 400)
deviation_palette <- c(
  "#DCEAF6", "#9ECAE1", "#4292C6", "#FEE08B",
  "#FDAE61", "#F46D43", "#B2182B"
)
legend_breaks <- legend_breaks[
  legend_breaks >= colour_limits[1] & legend_breaks <= colour_limits[2]
]

# Shared display range for the Male/Female age-specific IQR summaries. The
# range is based only on the box limits (Q1–Q3), with padding on a linear scale.
age_box_summary <- scores |>
  filter(Age != "PerCapita") |>
  group_by(Sex, Age) |>
  summarise(
    q1 = quantile(deviation_percent, 0.25),
    median = median(deviation_percent),
    q3 = quantile(deviation_percent, 0.75),
    .groups = "drop"
  ) |>
  mutate(
    Age = factor(Age, levels = age_levels, ordered = TRUE),
    age_index = as.numeric(Age)
  )
box_core_range <- range(age_box_summary$q1, age_box_summary$q3)
box_padding <- diff(box_core_range) * 0.07
box_y_limits <- box_core_range + c(-box_padding, box_padding)
box_y_breaks <- c(100, 200, 300)
box_y_breaks <- box_y_breaks[
  box_y_breaks >= box_y_limits[1] & box_y_breaks <= box_y_limits[2]
]

theme_map <- function() {
  theme_void(base_size = 12, base_family = "Arial") +
    theme(
      plot.title = element_text(
        size = 12, face = "bold", colour = "#272727", hjust = 0,
        margin = margin(b = 0.5)
      ),
      plot.subtitle = element_text(
        size = 5.5, colour = "#4D4D4D", hjust = 0,
        margin = margin(b = 0.8)
      ),
      strip.text = element_text(
        size = 6.3, face = "bold", colour = "#272727",
        margin = margin(t = 0, b = 0)
      ),
      plot.tag = element_text(
        size = 12, face = "bold", family = "Arial", colour = "#272727"
      ),
      # Negative horizontal spacing compensates for the internal whitespace
      # imposed by coord_sf's fixed map aspect ratio.
      panel.spacing.x = grid::unit(-6.3, "mm"),
      panel.spacing.y = grid::unit(0.10, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 6, face = "bold"),
      legend.text = element_text(size = 5.5),
      legend.margin = margin(t = 0.5),
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5)
    )
}

make_map <- function(sex_name) {
  plot_data <- world_by_age |>
    left_join(
      scores |>
        filter(Sex == sex_name) |>
        mutate(Age = as.character(Age)) |>
        select(ISO3, Age, deviation_percent),
      by = c("ISO3", "Age")
    ) |>
    mutate(Age = factor(Age, levels = age_levels, ordered = TRUE))

  ggplot(plot_data) +
    geom_sf(
      aes(fill = deviation_percent),
      colour = "#FFFFFF", linewidth = 0.01
    ) +
    facet_wrap(
      vars(Age), ncol = 6, drop = FALSE,
      labeller = labeller(Age = as_labeller(age_display_labels))
    ) +
    scale_fill_gradientn(
      colours = deviation_palette,
      trans = "log10",
      limits = colour_limits,
      oob = scales::squish,
      breaks = legend_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      na.value = "#D9D9D9",
      guide = guide_colourbar(
        title = "Mean absolute relative change",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(75, "mm"),
        barheight = grid::unit(3, "mm"),
        ticks.colour = "#4D4D4D",
        frame.colour = "#B0B0B0"
      )
    ) +
    labs(
      title = sex_name,
      fill = "Mean absolute relative change"
    ) +
    coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
    theme_map()
}

make_per_capita_map <- function() {
  sex_levels <- c("Male", "Female")
  world_by_sex <- world[rep(seq_len(nrow(world)), times = length(sex_levels)), ]
  world_by_sex$Sex <- rep(sex_levels, each = nrow(world))

  plot_data <- world_by_sex |>
    left_join(
      scores |>
        filter(Age == "PerCapita") |>
        select(ISO3, Sex, deviation_percent),
      by = c("ISO3", "Sex")
    ) |>
    mutate(Sex = factor(Sex, levels = sex_levels, ordered = TRUE))

  ggplot(plot_data) +
    geom_sf(
      aes(fill = deviation_percent),
      colour = "#FFFFFF", linewidth = 0.01
    ) +
    facet_wrap(vars(Sex), ncol = 2, drop = FALSE) +
    scale_fill_gradientn(
      colours = deviation_palette,
      trans = "log10",
      limits = colour_limits,
      oob = scales::squish,
      breaks = legend_breaks,
      labels = label_number(accuracy = 1, suffix = "%"),
      na.value = "#D9D9D9",
      guide = guide_colourbar(
        title = "Mean absolute relative change",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(75, "mm"),
        barheight = grid::unit(3, "mm"),
        ticks.colour = "#4D4D4D",
        frame.colour = "#B0B0B0"
      )
    ) +
    labs(
      title = "Per capita",
      subtitle = paste0(
        "Mean |optimized - current| / current across 8 food groups; ",
        "Total excluded; values >=400% share the darkest colour; grey indicates no data"
      ),
      fill = "Mean absolute relative change"
    ) +
    coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
    theme_map()
}

make_age_boxplot <- function(sex_name) {
  box_summary <- age_box_summary |>
    filter(Sex == sex_name)

  shown_ages <- age_levels[c(1, 5, 9, 13, 17)]

  ggplot() +
    geom_hline(
      yintercept = 100,
      linetype = "22", linewidth = 0.28, colour = "#8D3342"
    ) +
    geom_rect(
      data = box_summary,
      aes(
        xmin = age_index - 0.28, xmax = age_index + 0.28,
        ymin = q1, ymax = q3
      ),
      fill = "#7B9FB2", colour = NA, alpha = 0.88
    ) +
    geom_point(
      data = box_summary,
      aes(x = age_index, y = median),
      shape = 21, size = 0.72, stroke = 0.16,
      fill = "#B2182B", colour = "white"
    ) +
    scale_x_continuous(
      limits = c(0.5, length(age_levels) + 0.5),
      breaks = match(shown_ages, age_levels),
      labels = age_display_labels[shown_ages],
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      breaks = box_y_breaks,
      labels = paste0(box_y_breaks / 100, "x")
    ) +
    coord_cartesian(ylim = box_y_limits, clip = "on") +
    labs(title = "Country distribution by age", x = NULL, y = NULL) +
    theme_classic(base_size = 5, base_family = "Arial") +
    theme(
      plot.title = element_text(
        size = 5.5, face = "bold", hjust = 0.5,
        margin = margin(b = 0.5)
      ),
      axis.text.x = element_text(size = 4.8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 4.8),
      axis.ticks = element_line(linewidth = 0.18, colour = "#4D4D4D"),
      axis.ticks.length = grid::unit(0.6, "mm"),
      axis.line = element_line(linewidth = 0.22, colour = "#4D4D4D"),
      panel.grid.major.y = element_line(linewidth = 0.18, colour = "#E5E5E5"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(t = 0.5, r = 0.8, b = 0.5, l = 0.5)
    )
}

# Final figure order: Per capita, Male age groups, Female age groups.
per_capita_map <- make_per_capita_map() +
  labs(tag = "a") +
  theme(legend.position = "none")
male_map_base <- make_map("Male") +
  labs(tag = "b") +
  theme(legend.position = "none")
female_map_base <- make_map("Female") +
  labs(tag = "c") +
  theme(legend.position = "bottom")

male_map <- male_map_base +
  inset_element(
    make_age_boxplot("Male"),
    left = 5 / 6, right = 0.995, bottom = 0.01, top = 0.325,
    align_to = "panel", clip = FALSE
  )
female_map <- female_map_base +
  inset_element(
    make_age_boxplot("Female"),
    left = 5 / 6, right = 0.995, bottom = 0.01, top = 0.325,
    align_to = "panel", clip = FALSE
  )

final_figure <- per_capita_map / male_map / female_map +
  plot_layout(heights = c(0.72, 1.10, 1.10))

# Export the same final patchwork object to both formats. Using explicit
# devices plus print() ensures that patchwork insets (the age IQR summaries)
# are drawn on every device rather than relying on the state of an IDE viewer.
final_width_in <- 183 / 25.4
final_height_in <- 170 / 25.4
final_png <- file.path(output_dir, "structure_deviation_all.png")
final_svg <- file.path(output_dir, "structure_deviation_all.svg")

ragg::agg_png(
  final_png,
  width = final_width_in,
  height = final_height_in,
  units = "in",
  res = 600,
  background = "white"
)
print(final_figure)
grDevices::dev.off()

svglite::svglite(
  final_svg,
  width = final_width_in,
  height = final_height_in,
  bg = "white"
)
print(final_figure)
grDevices::dev.off()

message("Saved PNG: ", normalizePath(final_png))
message("Saved SVG: ", normalizePath(final_svg))
