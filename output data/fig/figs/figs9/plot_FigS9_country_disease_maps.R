#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(svglite)
})

# Resolve every path from this script so the workflow is reproducible from
# any working directory.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) == 1L) {
  normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else {
  normalizePath("plot_FigS9_country_disease_maps.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)

input_path <- file.path(script_dir, "figs9.xlsx")
world_path <- file.path(dirname(script_dir), "world.geojson")
output_dir <- file.path(script_dir, "FigS9_country_disease_maps_output")
output_stem <- file.path(output_dir, "FigS9_country_disease_maps")

for (path in c(input_path, world_path)) {
  if (!file.exists(path)) stop("Missing input file: ", path, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# Display order and colours follow the supplied disease key. Each endpoint has
# its own sequential family, while non-positive net effects remain neutral.
endpoint_info <- tibble::tribble(
  ~Endpoint,                    ~Label,                ~Panel, ~Pale,      ~Base,      ~Dark,
  "Ischemic heart disease",     "CHD",                 "a",    "#F3DEDC", "#B64C4A", "#6F2526",
  "Total cancers",              "Total cancers",       "b",    "#E6E1F1", "#7164A5", "#3F356E",
  "Diabetes mellitus type 2",  "T2D",                 "c",    "#D9ECEC", "#3E8D91", "#1E5E62",
  "Stroke",                     "Stroke",              "d",    "#DCE7F1", "#5B88B1", "#315A7D",
  "Colon and rectum cancer",    "Colorectal cancer",   "e",    "#F5E4CD", "#D8892D", "#8D4E12"
)

required_columns <- c(
  "ISO3", "Sex", "age_group", "Food group", "Endpoint", "delta_deaths"
)
deaths <- read_excel(input_path, sheet = "delta_deaths_summary")
missing_columns <- setdiff(required_columns, names(deaths))
assert_true(
  length(missing_columns) == 0L,
  paste0("Missing required column(s): ", paste(missing_columns, collapse = ", "))
)

deaths <- deaths %>%
  transmute(
    ISO3 = as.character(ISO3),
    Sex = as.character(Sex),
    age_group = as.character(age_group),
    Food_group = as.character(.data[["Food group"]]),
    Endpoint = as.character(Endpoint),
    delta_deaths = as.numeric(delta_deaths)
  )

assert_true(nrow(deaths) == 76284L, "Unexpected source-data row count.")
assert_true(!anyNA(deaths), "Missing values were found in the required source fields.")
assert_true(
  all(is.finite(deaths$delta_deaths)),
  "Non-finite delta_deaths values were found."
)
assert_true(
  setequal(unique(deaths$Endpoint), endpoint_info$Endpoint),
  "Disease endpoints differ from the expected five endpoints."
)
assert_true(
  n_distinct(deaths$ISO3) == 163L,
  "Expected 163 countries in the source data."
)

# Sum all supplied sex, age and food-group contributions within each country
# and endpoint. Endpoints remain separate; no disease totals are combined.
country_endpoint <- deaths %>%
  group_by(ISO3, Endpoint) %>%
  summarise(delta_deaths = sum(delta_deaths), .groups = "drop") %>%
  left_join(endpoint_info, by = "Endpoint") %>%
  arrange(match(Endpoint, endpoint_info$Endpoint), ISO3)

assert_true(
  nrow(country_endpoint) == 163L * nrow(endpoint_info),
  "Expected one national value for each of 163 countries and five endpoints."
)
assert_true(
  !anyDuplicated(country_endpoint[c("ISO3", "Endpoint")]),
  "Duplicate country-endpoint totals were produced."
)

# Read and harmonize the supplied country boundaries. Antarctica is omitted
# from the visual frame; no source-data row is removed.
previous_s2 <- sf_use_s2()
sf_use_s2(FALSE)
world <- st_read(world_path, quiet = TRUE) %>%
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) %>%
  mutate(ISO3 = recode(as.character(SOC), ROM = "ROU", TMP = "TLS")) %>%
  st_make_valid() %>%
  suppressWarnings(st_break_antimeridian(lon_0 = 0, tol = 1e-4))
sf_use_s2(previous_s2)

unmatched_iso3 <- setdiff(unique(country_endpoint$ISO3), unique(world$ISO3))
assert_true(
  length(unmatched_iso3) == 0L,
  paste0("Country code(s) missing from the map: ", paste(unmatched_iso3, collapse = ", "))
)

format_deaths <- function(x) {
  ifelse(
    abs(x) >= 1e6,
    sub("\\.0M$", "M", sprintf("%.1fM", x / 1e6)),
    ifelse(
      abs(x) >= 1e3,
      sub("\\.0K$", "K", sprintf("%.1fK", x / 1e3)),
      ifelse(
        abs(x) >= 100,
        format(round(x), scientific = FALSE, trim = TRUE, big.mark = ","),
        sub("\\.0$", "", sprintf("%.1f", x))
      )
    )
  )
}

make_positive_classes <- function(values) {
  positive <- values[values > 0]
  assert_true(length(positive) >= 5L, "Too few positive values for five classes.")
  internal_breaks <- as.numeric(
    quantile(positive, probs = seq(0.2, 0.8, by = 0.2), type = 7)
  )
  assert_true(
    length(unique(internal_breaks)) == 4L,
    "Positive quantile breaks are not unique."
  )

  labels <- c(
    "≤0",
    ">0",
    paste0(">", format_deaths(internal_breaks[1])),
    paste0(">", format_deaths(internal_breaks[2])),
    paste0(">", format_deaths(internal_breaks[3])),
    paste0(">", format_deaths(internal_breaks[4]))
  )

  list(
    breaks = c(-Inf, 0, internal_breaks, Inf),
    labels = labels,
    internal_breaks = internal_breaks
  )
}

classify_endpoint <- function(endpoint_data) {
  class_info <- make_positive_classes(endpoint_data$delta_deaths)
  classified_data <- endpoint_data %>%
    mutate(
      Display_class = cut(
        delta_deaths,
        breaks = class_info$breaks,
        labels = class_info$labels,
        include.lowest = TRUE,
        right = TRUE,
        ordered_result = TRUE
      )
    )
  list(data = classified_data, class_info = class_info)
}

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
nonpositive_colour <- "#B8B8B8"
missing_colour <- "#ECEDEB"
outline_colour <- "#FFFFFF"

make_map <- function(endpoint) {
  info <- endpoint_info %>% filter(Endpoint == endpoint)
  classified <- classify_endpoint(
    country_endpoint %>% filter(Endpoint == endpoint)
  )
  map_data <- world %>% left_join(classified$data, by = "ISO3")
  positive_colours <- grDevices::colorRampPalette(
    c(info$Pale, info$Base, info$Dark), space = "Lab"
  )(5)
  fill_values <- setNames(
    c(nonpositive_colour, positive_colours),
    classified$class_info$labels
  )

  ggplot(map_data) +
    geom_sf(
      aes(fill = Display_class),
      colour = outline_colour,
      linewidth = 0.10
    ) +
    coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
    scale_fill_manual(
      values = fill_values,
      limits = classified$class_info$labels,
      drop = FALSE,
      na.value = missing_colour,
      name = "Net deaths avoided"
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        title.position = "top",
        title.hjust = 0,
        keywidth = grid::unit(11.5, "mm"),
        keyheight = grid::unit(3.2, "mm")
      )
    ) +
    labs(title = info$Label, tag = info$Panel) +
    theme_void(base_family = "Arial", base_size = 12) +
    theme(
      text = element_text(family = "Arial", colour = "#202020"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(
        size = 12, face = "bold", hjust = 0,
        margin = margin(l = 7, b = 0.2, unit = "mm")
      ),
      plot.tag = element_text(size = 12, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = c(0.006, 0.995),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = element_text(size = 12, face = "plain"),
      legend.text = element_text(size = 12, hjust = 0.5),
      legend.text.position = "bottom",
      legend.spacing.x = grid::unit(0.2, "mm"),
      legend.spacing.y = grid::unit(0.5, "mm"),
      legend.key.spacing.x = grid::unit(0.7, "mm"),
      legend.key.spacing.y = grid::unit(0.8, "mm"),
      legend.margin = margin(t = 0, b = 0, unit = "mm"),
      legend.box.margin = margin(0, 0, 0, 0, unit = "mm"),
      plot.margin = margin(0.5, 0.7, 0.5, 0.7, unit = "mm")
    )
}

save_svg <- function(plot, stem, width_mm, height_mm) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  svglite::svglite(
    paste0(stem, ".svg"), width = width_in, height = height_in,
    bg = "white"
  )
  print(plot)
  grDevices::dev.off()
}

# Assemble one two-column supplementary figure; the fifth panel is centred.
panel_maps <- lapply(endpoint_info$Endpoint, make_map)
combined_design <- c(
  area(t = 1, l = 1, b = 1, r = 2),
  area(t = 1, l = 3, b = 1, r = 4),
  area(t = 2, l = 1, b = 2, r = 2),
  area(t = 2, l = 3, b = 2, r = 4),
  area(t = 3, l = 2, b = 3, r = 3)
)
figure_body <- wrap_plots(
  panel_maps,
  design = combined_design,
  heights = c(1, 1, 1)
)
combined <- figure_body

save_svg(
  combined,
  output_stem,
  width_mm = 183,
  height_mm = 174
)

message("Saved the combined Fig. S9 SVG to: ", output_dir)
