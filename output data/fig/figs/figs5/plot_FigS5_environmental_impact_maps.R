#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# Resolve paths from the script location so the script works from any folder.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) == 1L) {
  normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else {
  normalizePath("plot_FigS5_environmental_impact_maps.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)

input_path <- file.path(
  script_dir,
  "sensitivity-EAT-Lancet-environmental-impact.xlsx"
)
world_path <- file.path(dirname(script_dir), "world.geojson")
output_dir <- file.path(script_dir, "FigS5_environmental_impact_maps_output")
output_stem <- file.path(output_dir, "FigS5_environmental_impact_maps")

for (path in c(input_path, world_path)) {
  if (!file.exists(path)) stop("Missing input file: ", path, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

environment_levels <- c("GHG", "Land", "Freshwater", "Eutr.", "Acid.")
metric_info <- tibble(
  Environment = environment_levels,
  RowLabel = c(
    "GHG emissions", "Land use", "Water use",
    "Eutrophication", "Acidification"
  ),
  Unit = c("Mt", "Mha", "km³", "kt", "kt"),
  DisplayMultiplier = c(1000, 1, 1, 1000, 1000)
)

country <- read_excel(input_path, sheet = "Country_Comparison")
required_columns <- c(
  "ISO3", "Environment", "CurrentImpact", "EATImpact", "ChangePct"
)
missing_columns <- setdiff(required_columns, names(country))
assert_true(
  length(missing_columns) == 0L,
  paste0(
    "Country_Comparison is missing required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
)

country <- country %>%
  transmute(
    ISO3 = as.character(ISO3),
    Environment = as.character(Environment),
    CurrentImpact = as.numeric(CurrentImpact),
    EATImpact = as.numeric(EATImpact),
    ChangePct = as.numeric(ChangePct)
  )

assert_true(
  setequal(unique(country$Environment), environment_levels),
  "Environmental indicators do not match the requested five-row figure."
)
assert_true(
  !anyDuplicated(country[c("ISO3", "Environment")]),
  "Duplicate ISO3-Environment rows were found in Country_Comparison."
)
assert_true(
  n_distinct(country$ISO3) == 163L && nrow(country) == 815L,
  "Expected 163 countries x 5 environmental indicators (815 rows)."
)
assert_true(
  !anyNA(country[required_columns]),
  "Missing country-level impact or percentage-change value."
)
assert_true(
  all(country$CurrentImpact >= 0) && all(country$EATImpact >= 0),
  "Absolute environmental impacts must be non-negative."
)

country <- country %>%
  mutate(Environment = factor(Environment, levels = environment_levels))

# Read and harmonize the supplied country boundaries. Two legacy country codes
# in the map are updated to their current ISO3 equivalents.
previous_s2 <- sf_use_s2()
sf_use_s2(FALSE)
world <- st_read(world_path, quiet = TRUE) %>%
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) %>%
  mutate(ISO3 = recode(as.character(SOC), ROM = "ROU", TMP = "TLS")) %>%
  st_make_valid() %>%
  suppressWarnings(st_break_antimeridian(lon_0 = 0, tol = 1e-4))
sf_use_s2(previous_s2)

unmatched_iso3 <- setdiff(unique(country$ISO3), unique(world$ISO3))
assert_true(
  length(unmatched_iso3) == 0L,
  paste0("Country code(s) missing from the map: ", paste(unmatched_iso3, collapse = ", "))
)

absolute_long <- country %>%
  pivot_longer(
    cols = c(CurrentImpact, EATImpact),
    names_to = "Scenario",
    values_to = "Impact"
  ) %>%
  left_join(
    metric_info %>% select(Environment, DisplayMultiplier),
    by = "Environment"
  ) %>%
  mutate(
    # The workbook stores GHG in Gt; national maps display it in Mt.
    Impact = Impact * DisplayMultiplier,
    Scenario = recode(
      Scenario,
      CurrentImpact = "Current",
      EATImpact = "EAT"
    ),
    Scenario = factor(Scenario, levels = c("Current", "EAT"), ordered = TRUE)
  )

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
missing_colour <- "#D9D9D9"

# Equal-width discrete legend keys. Each numeric label is the lower bound of
# its colour class; the final label also represents all larger values.
absolute_thresholds <- list(
  "GHG" = c(0, 5, 10, 25, 50, 75, 100, 250, 500, 750, 1000, 1500),
  "Land" = c(0, 0.5, 1, 2.5, 5, 10, 25, 50, 75, 100, 250, 500),
  "Freshwater" = c(0, 0.5, 1, 2.5, 5, 10, 25, 50, 75, 100, 250, 500),
  "Eutr." = c(0, 10, 25, 50, 75, 100, 250, 500, 750, 1000, 1500, 5000),
  "Acid." = c(0, 10, 25, 50, 75, 100, 250, 500, 750, 1000, 1500, 5000)
)

absolute_colours <- c(
  "#FFF4D6", "#F3EAB0", "#DFE3A0", "#C4D99A",
  "#A6CF9D", "#83C1A1", "#66B1A4", "#56A0A8",
  "#568FAB", "#627DAC", "#716CA8", "#7F5E9E"
)
change_colours <- c(
  "#537CA6", "#7EA4C7", "#B5CDE0", "#F4F1EC",
  "#EBC5B6", "#DC9782", "#C96B68"
)

format_threshold <- function(x) {
  ifelse(
    abs(x - round(x)) < 1e-10,
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE),
    format(x, nsmall = 1, big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

classify_by_lower_bound <- function(x, thresholds, labels) {
  index <- findInterval(x, thresholds, all.inside = TRUE)
  labels[index]
}

for (environment in environment_levels) {
  assert_true(
    length(absolute_thresholds[[environment]]) == length(absolute_colours),
    paste0("Threshold/colour count mismatch for ", environment, ".")
  )
}

# The percentage-change maps use the user-specified common display range.
# Values outside +/-50% retain their source values but share terminal classes.
change_limit <- 50

absolute_caps <- absolute_long %>%
  group_by(Environment) %>%
  summarise(
    Cap95Raw = as.numeric(quantile(Impact, 0.95, na.rm = TRUE, type = 7)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    DisplayCap = max(absolute_thresholds[[as.character(Environment)]]),
    ValuesAboveCap = sum(
      absolute_long$Environment == Environment &
        absolute_long$Impact >= DisplayCap
    )
  ) %>%
  ungroup()

theme_map <- function() {
  theme_void(base_family = "Arial") +
    theme(
      text = element_text(family = "Arial", colour = "#252525"),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = element_text(size = 12, face = "bold", margin = margin(1.2, 0, 1.2, 0)),
      # Slight overlap enlarges the geographic body within the 230-mm canvas.
      panel.spacing.x = grid::unit(-12, "mm"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(
        size = 12, face = "plain", hjust = 0,
        margin = margin(b = 5)
      ),
      legend.text = element_text(size = 12, margin = margin(t = 4)),
      legend.spacing.x = grid::unit(0, "mm"),
      legend.key.spacing = grid::unit(0, "mm"),
      legend.key.spacing.x = grid::unit(0, "mm"),
      legend.margin = margin(t = 0.4, b = 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = margin(-1, -5, -1, -5, unit = "mm")
    )
}

make_header_box <- function(label) {
  ggplot() +
    annotate(
      "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
      fill = "#F2F2F2", colour = "#8F8F8F", linewidth = 0.35
    ) +
    annotate(
      "text", x = 0.5, y = 0.5, label = label,
      family = "Arial", fontface = "bold", size = 12 / 2.845,
      colour = "#252525"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0.5, 0, 0.5))
}

make_row_strip <- function(label) {
  ggplot() +
    annotate(
      "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
      fill = "#F2F2F2", colour = "#8F8F8F", linewidth = 0.35
    ) +
    annotate(
      "text", x = 0.5, y = 0.5, label = label, angle = 90,
      family = "Arial", fontface = "bold", size = 12 / 2.845,
      lineheight = 0.92, colour = "#252525"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0.4, 0.5, 0.2, 0.5))
}

make_absolute_legend <- function(threshold_labels, legend_title) {
  legend_data <- tibble(
    index = seq_along(threshold_labels),
    xmin = seq_along(threshold_labels) - 1,
    xmax = seq_along(threshold_labels),
    label = threshold_labels,
    colour = absolute_colours
  )

  ggplot(legend_data) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = 0.54, ymax = 0.72, fill = colour),
      colour = NA
    ) +
    geom_text(
      aes(x = index - 0.5, y = 0.27, label = label),
      family = "Arial", size = 12 / 2.845, colour = "#252525"
    ) +
    annotate(
      "text", x = 0, y = 0.98, label = legend_title,
      hjust = 0, vjust = 1, family = "Arial", size = 12 / 2.845,
      colour = "#252525"
    ) +
    scale_fill_identity() +
    coord_cartesian(
      xlim = c(0, length(threshold_labels)), ylim = c(0.05, 1),
      expand = FALSE, clip = "off"
    ) +
    theme_void(base_family = "Arial") +
    theme(plot.margin = margin(0.5, 1.5, 0, 1.5, unit = "mm"))
}

make_change_legend <- function() {
  legend_data <- tibble(
    value = seq(-change_limit, change_limit, length.out = 401)
  )
  legend_ticks <- tibble(
    value = c(-50, -25, 0, 25, 50),
    label = c("≤−50", "−25", "0", "25", "≥50"),
    hjust = c(0, 0.5, 0.5, 0.5, 1)
  )

  ggplot(legend_data) +
    geom_tile(
      aes(x = value, y = 0.63, fill = value),
      width = 2 * change_limit / 400, height = 0.18
    ) +
    geom_segment(
      data = legend_ticks,
      aes(x = value, xend = value, y = 0.54, yend = 0.72),
      inherit.aes = FALSE, linewidth = 0.25, colour = "#555555"
    ) +
    geom_text(
      data = legend_ticks,
      aes(x = value, y = 0.27, label = label, hjust = hjust),
      inherit.aes = FALSE, family = "Arial", size = 12 / 2.845,
      colour = "#252525"
    ) +
    annotate(
      "text", x = -change_limit, y = 0.98, label = "National change (%)",
      hjust = 0, vjust = 1, family = "Arial", size = 12 / 2.845,
      colour = "#252525"
    ) +
    scale_fill_gradientn(
      colours = change_colours,
      values = seq(0, 1, length.out = length(change_colours)),
      limits = c(-change_limit, change_limit),
      guide = "none"
    ) +
    coord_cartesian(
      xlim = c(-change_limit, change_limit), ylim = c(0.05, 1),
      expand = FALSE, clip = "off"
    ) +
    theme_void(base_family = "Arial") +
    theme(plot.margin = margin(0.5, 1.5, 0, 1.5, unit = "mm"))
}

make_absolute_maps <- function(environment, row_label, unit) {
  values <- absolute_long %>% filter(Environment == environment)
  thresholds <- absolute_thresholds[[as.character(environment)]]
  threshold_labels <- format_threshold(thresholds)

  legend_title <- switch(
    as.character(environment),
    "GHG" = "National GHG emissions (MtCO₂e)",
    "Land" = "National land use (Mha)",
    "Freshwater" = "National water use (km³)",
    "Eutr." = "National eutrophication (kt PO₄-eq)",
    "Acid." = "National acidification (kt SO₂-eq)"
  )

  world_scenario <- world[rep(seq_len(nrow(world)), times = 2L), ]
  world_scenario$Scenario <- rep(c("Current", "EAT"), each = nrow(world))

  plot_data <- world_scenario %>%
    mutate(Scenario = factor(Scenario, levels = c("Current", "EAT"), ordered = TRUE)) %>%
    left_join(
      values %>% select(ISO3, Scenario, Impact),
      by = c("ISO3", "Scenario")
    ) %>%
    mutate(
      ImpactClass = if_else(
        is.na(Impact),
        NA_character_,
        classify_by_lower_bound(Impact, thresholds, threshold_labels)
      ),
      ImpactClass = factor(
        ImpactClass, levels = threshold_labels, ordered = TRUE
      )
    )

  map <- ggplot(plot_data) +
    geom_sf(aes(fill = ImpactClass), colour = "#FFFFFF", linewidth = 0.035) +
    facet_wrap(vars(Scenario), nrow = 1, drop = FALSE) +
    scale_fill_manual(
      values = setNames(absolute_colours, threshold_labels),
      limits = threshold_labels,
      drop = FALSE,
      na.value = missing_colour,
      na.translate = FALSE,
      guide = "none"
    ) +
    coord_sf(crs = moll_crs, datum = NA, expand = FALSE, clip = "off") +
    theme_map() +
    theme(
      strip.text = element_blank(),
      strip.background = element_blank()
    )

  list(
    map = map,
    legend = make_absolute_legend(threshold_labels, legend_title)
  )
}

make_change_map <- function(environment) {
  values <- country %>%
    filter(Environment == environment) %>%
    select(ISO3, ChangePct)

  plot_data <- world %>% left_join(values, by = "ISO3")

  map <- ggplot(plot_data) +
    geom_sf(aes(fill = ChangePct), colour = "#FFFFFF", linewidth = 0.035) +
    scale_fill_gradientn(
      colours = change_colours,
      values = seq(0, 1, length.out = length(change_colours)),
      limits = c(-change_limit, change_limit),
      breaks = c(-50, -25, 0, 25, 50),
      labels = c("≤−50", "−25", "0", "25", "≥50"),
      oob = squish,
      na.value = missing_colour,
      guide = "none"
    ) +
    coord_sf(crs = moll_crs, datum = NA, expand = FALSE, clip = "off") +
    theme_map()

  list(map = map, legend = make_change_legend())
}

row_plots <- lapply(seq_len(nrow(metric_info)), function(i) {
  environment <- metric_info$Environment[[i]]
  row_label <- metric_info$RowLabel[[i]]
  unit <- metric_info$Unit[[i]]

  absolute_maps <- make_absolute_maps(environment, row_label, unit)
  change_map <- make_change_map(environment)
  row_strip <- make_row_strip(row_label)

  row_design <- c(
    patchwork::area(t = 1, l = 1, b = 1, r = 2),
    patchwork::area(t = 1, l = 3, b = 1, r = 3),
    patchwork::area(t = 1, l = 4, b = 2, r = 4),
    patchwork::area(t = 2, l = 1, b = 2, r = 2),
    patchwork::area(t = 2, l = 3, b = 2, r = 3)
  )

  wrap_plots(
    absolute_maps$map,
    change_map$map,
    row_strip,
    absolute_maps$legend,
    change_map$legend,
    design = row_design,
    widths = c(1, 1, 1, 0.07),
    heights = c(1, 0.22)
  )
})

header_row <-
  (
    make_header_box("Current") |
      make_header_box("EAT") |
      make_header_box("Change (%)") |
      plot_spacer()
  ) +
  plot_layout(widths = c(1, 1, 1, 0.07))

figure <- wrap_plots(
  c(list(header_row), row_plots),
  ncol = 1,
  heights = c(0.14, rep(1, length(row_plots)))
)

# Width is capped at the user-specified 230 mm. Negative panel spacing and
# clipping outside individual panels enlarge the maps without dropping data.
width_mm <- 230
height_mm <- 280
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(
  paste0(output_stem, ".svg"),
  width = width_in, height = height_in, bg = "white"
)
print(figure)
dev.off()

grDevices::cairo_pdf(
  paste0(output_stem, ".pdf"),
  width = width_in, height = height_in,
  family = "Arial", bg = "white"
)
print(figure)
dev.off()

ragg::agg_tiff(
  paste0(output_stem, ".tiff"),
  width = width_in, height = height_in,
  units = "in", res = 600, background = "white", compression = "lzw"
)
print(figure)
dev.off()

ragg::agg_png(
  paste0(output_stem, ".png"),
  width = width_in, height = height_in,
  units = "in", res = 300, background = "white"
)
print(figure)
dev.off()

source_data <- country %>%
  left_join(metric_info, by = "Environment") %>%
  left_join(absolute_caps %>% select(Environment, DisplayCap), by = "Environment") %>%
  transmute(
    ISO3,
    Environment = as.character(Environment),
    RowLabel,
    Unit,
    CurrentImpact = CurrentImpact * DisplayMultiplier,
    EATImpact = EATImpact * DisplayMultiplier,
    ChangePct,
    AbsoluteColourCap = DisplayCap,
    ChangeColourLower = -change_limit,
    ChangeColourUpper = change_limit
  ) %>%
  rowwise() %>%
  mutate(
    CurrentColourClass = classify_by_lower_bound(
      CurrentImpact,
      absolute_thresholds[[Environment]],
      format_threshold(absolute_thresholds[[Environment]])
    ),
    EATColourClass = classify_by_lower_bound(
      EATImpact,
      absolute_thresholds[[Environment]],
      format_threshold(absolute_thresholds[[Environment]])
    ),
    CurrentColour = absolute_colours[
      match(
        CurrentColourClass,
        format_threshold(absolute_thresholds[[Environment]])
      )
    ],
    EATColour = absolute_colours[
      match(
        EATColourClass,
        format_threshold(absolute_thresholds[[Environment]])
      )
    ]
  ) %>%
  ungroup() %>%
  arrange(match(Environment, environment_levels), ISO3)

write.csv(
  source_data,
  file.path(output_dir, "FigS5_environmental_impact_maps_source_data.csv"),
  row.names = FALSE,
  na = ""
)

colour_key <- bind_rows(lapply(environment_levels, function(environment) {
  thresholds <- absolute_thresholds[[environment]]
  tibble(
    Environment = environment,
    LowerBoundInclusive = thresholds,
    UpperBoundExclusive = c(thresholds[-1], Inf),
    LegendLabel = format_threshold(thresholds),
    Colour = absolute_colours
  )
}))

write.csv(
  colour_key,
  file.path(output_dir, "FigS5_environmental_impact_colour_key.csv"),
  row.names = FALSE,
  na = ""
)

qa_lines <- c(
  "Figure contract",
  "Core conclusion: national environmental impacts generally decline under the EAT diet, with spatial heterogeneity and some country-level increases.",
  "Evidence chain: rows are five indicators shown as right-side vertical facet strips; columns are Current, EAT, and percentage change with one shared header row.",
  "Archetype: quantitative grid (5 rows x 3 map columns).",
  "Backend: R only (sf, ggplot2, patchwork).",
  "",
  "Data integrity",
  paste0("Input observations: ", nrow(country), " (", n_distinct(country$ISO3), " countries x 5 indicators)."),
  "Excluded observations: 0.",
  paste0("Country codes matched to map: ", n_distinct(country$ISO3), "/", n_distinct(country$ISO3), "."),
  "Map territories without study data are grey; Antarctica is omitted from the display only.",
  "",
  "Scale contract",
  "Current and EAT share the same 0-to-maximum scale within each indicator.",
  "Absolute impacts are classified before plotting; every threshold class is bound one-to-one to one fixed colour in both map and legend.",
  "Absolute-impact legends use equal-width, contiguous colour blocks; values at or above the final threshold share the darkest colour.",
  paste0(
    "Absolute caps: ",
    paste0(
      metric_info$RowLabel, "=",
      absolute_caps$DisplayCap[match(metric_info$Environment, absolute_caps$Environment)],
      " ", metric_info$Unit,
      collapse = "; "
    ), "."
  ),
  paste0("All change maps use a common continuous scale spanning -", change_limit, "% to +", change_limit, "% with terminal-colour squishing."),
  paste0("Change values below -", change_limit, "% or above +", change_limit, "% retain their source values but share the endpoint colour."),
  "All visible text is 12 pt.",
  "Map panels use negative horizontal spacing and clip-off rendering to enlarge the geographic body; slight panel overlap is intentional.",
  "Units: GHG=Mt; Land=Mha; Water=km3; Eutro.=kt; Acid.=kt.",
  "",
  "Export",
  paste0("Final size: ", width_mm, " x ", height_mm, " mm."),
  "Outputs: editable SVG and PDF; 600-dpi LZW TIFF; 300-dpi PNG preview.",
  "Grey indicates no study data."
)
writeLines(
  qa_lines,
  file.path(output_dir, "FigS5_environmental_impact_maps_QA.txt")
)

message("Figure written to: ", output_dir)
message(
  "Countries=", n_distinct(country$ISO3),
  "; indicators=", n_distinct(country$Environment),
  "; change scale=+/-", change_limit, "%"
)
