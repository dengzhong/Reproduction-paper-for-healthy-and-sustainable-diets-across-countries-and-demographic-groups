#!/usr/bin/env Rscript

# Figure S4 contract
# Core conclusion: adopting the EAT-Lancet diet redistributes per-capita food
# costs unevenly across countries and income groups, with the net change driven
# by distinct food-group contributions.
# Evidence chain: (a) national percentage change and direction counts;
# (b) food-group shares of current/EAT per-capita costs; (c) signed food-group
# contributions to the EAT-current per-capita cost difference.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  library(scales)
})

required_packages <- "svglite"
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "))
}

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- command_args[grepl("^--file=", command_args)]
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("plot_FigS4_food_cost.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)
world_path <- file.path(dirname(script_dir), "world.geojson")
cost_path <- file.path(script_dir, "sensitivity-EAT-Lancet-food-cost.xlsx")
output_prefix <- file.path(script_dir, "FigS4_food_cost")

if (!file.exists(world_path)) stop("Missing map file: ", world_path)
if (!file.exists(cost_path)) stop("Missing food-cost workbook: ", cost_path)

food_groups <- list(
  Seafood = c(
    "fish_freshw", "fish_demrs", "fish_pelag", "fish_marine",
    "crustaceans", "cephalopods", "othr_molluscs", "fish_aquatic",
    "fish_aquaticplant"
  ),
  `Dairy & eggs` = c("milk", "eggs"),
  Meat = c("beef", "lamb", "pork", "othr_meat", "offals", "poultry"),
  `Sugar & oil` = c(
    "sugar_cane", "sugar_non", "raw_sugar", "sweeteners", "honey",
    "oil_soyabeans", "oil_groundnut", "oil_sunflower", "oil_rape",
    "oil_cotton", "oil_palmkernel", "oil_palm", "oil_coconut",
    "oil_sesame", "oil_olive", "oil_ricebran", "oil_maize",
    "oil_oilcrop", "butter", "cream", "fat_ani", "oil_fish_body",
    "oil_fish_liver"
  ),
  `Staple foods` = c(
    "wheat", "rice", "barley", "maize", "rye", "oats", "millet",
    "sorghum", "othr_grains", "cassava", "potato", "sweet_potato",
    "yams", "othr_roots"
  ),
  `Legumes & nuts` = c(
    "beans", "peas", "othr_pulse", "soyabeans", "nuts", "groundnut",
    "seed_sunflower", "seed_rape", "seed_cotton", "seed_sesame",
    "seed_oilcrop"
  ),
  Fruits = c(
    "coconuts", "preserved_olives", "orange", "lemon", "grapefruit",
    "citrus", "banana", "plantains", "apple", "pineapple", "dates",
    "grapes", "othr_fruits"
  ),
  Vegetable = c("tomato", "onion", "othr_vegetables")
)

food_group_levels <- names(food_groups)
food_lookup <- bind_rows(lapply(names(food_groups), function(group_name) {
  tibble(Food = food_groups[[group_name]], FoodGroup = group_name)
})) %>%
  mutate(FoodGroup = factor(FoodGroup, levels = food_group_levels, ordered = TRUE))

if (anyDuplicated(food_lookup$Food)) {
  duplicated_foods <- unique(food_lookup$Food[duplicated(food_lookup$Food)])
  stop("Food item(s) assigned to multiple groups: ", paste(duplicated_foods, collapse = ", "))
}

country_summary <- read_excel(cost_path, sheet = "Country_summary")
global_food <- read_excel(cost_path, sheet = "Global_food_detail")
income_food <- read_excel(cost_path, sheet = "Income_food_detail")
global_summary <- read_excel(cost_path, sheet = "Global_summary")
income_summary <- read_excel(cost_path, sheet = "Income_summary")
food_detail <- read_excel(cost_path, sheet = "Food_cost_detail")

required_country_cols <- c(
  "ISO3", "IncomeLevel", "Population", "CurrentCostPerCapitaDay",
  "EATCostPerCapitaDay", "Difference", "ChangePct"
)
if (!all(required_country_cols %in% names(country_summary))) {
  stop("Country_summary is missing required columns.")
}

workbook_foods <- sort(unique(food_detail$Food))
grouped_foods <- sort(food_lookup$Food)
if (!identical(workbook_foods, grouped_foods)) {
  stop(
    "Food-group definitions do not exactly cover the workbook foods. Missing: ",
    paste(setdiff(workbook_foods, grouped_foods), collapse = ", "),
    "; extra: ", paste(setdiff(grouped_foods, workbook_foods), collapse = ", ")
  )
}

if (nrow(food_lookup) != 81L || n_distinct(country_summary$ISO3) != 163L) {
  stop("Expected 81 foods and 163 countries after validation.")
}

geo_levels <- c("W", "H", "UM", "LM", "L")
income_abbrev <- c(
  "High income" = "H",
  "Upper-middle income" = "UM",
  "Lower middle income" = "LM",
  "Low income" = "L"
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
direction_palette <- c("Cost decrease" = "#2D6292", "Cost increase" = "#B53B4A")
missing_colour <- "#E3E3E3"

theme_nature <- function() {
  theme_classic(base_size = 12, base_family = "Helvetica") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "#000000"),
      axis.ticks = element_line(linewidth = 0.35, colour = "#000000"),
      axis.text = element_text(size = 12, colour = "#000000"),
      axis.title = element_text(size = 12, colour = "#000000"),
      plot.title = element_text(size = 10, face = "bold", hjust = 0, colour = "#000000"),
      plot.tag = element_text(size = 10, face = "bold", colour = "#000000"),
      strip.text = element_text(size = 10, face = "bold", colour = "#000000"),
      strip.background = element_blank(),
      legend.title = element_text(size = 12, colour = "#000000"),
      legend.text = element_text(size = 12, colour = "#000000"),
      panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Panel a: national percentage change map.
previous_s2 <- sf_use_s2()
sf_use_s2(FALSE)
world <- st_read(world_path, quiet = TRUE) %>%
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) %>%
  mutate(
    ISO3 = recode(as.character(SOC), ROM = "ROU", TMP = "TLS")
  ) %>%
  st_make_valid() %>%
  suppressWarnings(st_break_antimeridian(lon_0 = 0, tol = 1e-4)) %>%
  left_join(country_summary %>% select(ISO3, ChangePct), by = "ISO3")
sf_use_s2(previous_s2)

unmatched_iso3 <- setdiff(country_summary$ISO3, world$ISO3)
if (length(unmatched_iso3) > 0L) {
  stop("Country code(s) missing from map: ", paste(unmatched_iso3, collapse = ", "))
}

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
map_limit <- 60
p_map <- ggplot(world) +
  geom_sf(aes(fill = ChangePct), colour = "white", linewidth = 0.06) +
  scale_fill_gradient2(
    low = direction_palette[["Cost decrease"]],
    mid = "#F7F6F2",
    high = direction_palette[["Cost increase"]],
    midpoint = 0,
    limits = c(-map_limit, map_limit),
    oob = squish,
    breaks = c(-60, -30, 0, 30, 60),
    labels = c("−60", "−30", "0", "30", "≥60"),
    na.value = missing_colour,
    guide = guide_colourbar(
      title = "Change in per-capita food cost (%)",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(68, "mm"),
      barheight = grid::unit(2.8, "mm"),
      ticks.colour = "#000000",
      frame.colour = "#999999"
    )
  ) +
  coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, colour = "#000000"),
    legend.text = element_text(size = 12, colour = "#000000"),
    legend.margin = margin(t = 1, b = 0),
    plot.margin = margin(0, 0, 0, 0)
  )

donut_counts <- bind_rows(
  country_summary %>% mutate(Geography = "W"),
  country_summary %>% mutate(Geography = recode(IncomeLevel, !!!income_abbrev))
) %>%
  mutate(
    Direction = if_else(ChangePct < 0, "Cost decrease", "Cost increase"),
    Geography = factor(Geography, levels = geo_levels, ordered = TRUE),
    Direction = factor(Direction, levels = names(direction_palette), ordered = TRUE)
  ) %>%
  count(Geography, Direction, name = "Countries", .drop = FALSE)

make_donut <- function(label) {
  plot_data <- donut_counts %>% filter(Geography == label)
  ggplot(plot_data, aes(x = 2, y = Countries, fill = Direction)) +
    geom_col(width = 0.58, colour = "white", linewidth = 0.18) +
    geom_text(
      aes(x = 1.82, label = Countries),
      position = position_stack(vjust = 0.5),
      colour = "white", size = 10 / ggplot2::.pt,
      fontface = "bold", family = "Helvetica"
    ) +
    annotate(
      "text", x = 0.72, y = 0, label = label,
      size = 10 / ggplot2::.pt, fontface = "bold",
      family = "Helvetica", colour = "#000000"
    ) +
    coord_polar(theta = "y") +
    xlim(0.72, 2.45) +
    scale_fill_manual(values = direction_palette, drop = FALSE) +
    theme_void() +
    theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
}

direction_key <- tibble(
  Direction = factor(names(direction_palette), levels = names(direction_palette)),
  x = 1, y = c(2, 1)
)
p_direction_key <- ggplot(direction_key, aes(x, y, fill = Direction)) +
  geom_tile(width = 0.18, height = 0.42) +
  geom_text(
    aes(x = 1.14, label = Direction), hjust = 0,
    size = 12 / ggplot2::.pt, family = "Helvetica", colour = "#000000"
  ) +
  scale_fill_manual(values = direction_palette) +
  xlim(0.86, 2.45) +
  ylim(0.65, 2.35) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))

panel_a <- ggdraw(p_map) +
  draw_plot(make_donut("W"), x = 0.035, y = 0.29, width = 0.170, height = 0.235) +
  draw_plot(make_donut("H"), x = 0.210, y = 0.57, width = 0.170, height = 0.235) +
  draw_plot(make_donut("UM"), x = 0.340, y = 0.17, width = 0.170, height = 0.235) +
  draw_plot(make_donut("LM"), x = 0.620, y = 0.24, width = 0.170, height = 0.235) +
  draw_plot(make_donut("L"), x = 0.820, y = 0.58, width = 0.170, height = 0.235) +
  draw_plot(p_direction_key, x = 0.025, y = 0.10, width = 0.25, height = 0.12) +
  labs(tag = "a") +
  theme(
    plot.tag = element_text(size = 10, face = "bold", colour = "#000000"),
    plot.tag.position = c(0.005, 0.995)
  )

# Panels b/c: population-weighted food-group costs for World and income levels.
combined_food <- bind_rows(
  global_food %>%
    transmute(
      Geography = "W", Scenario, Food,
      CostPerCapitaDay = PopulationWeightedFoodCostPerCapitaDay
    ),
  income_food %>%
    transmute(
      Geography = recode(IncomeLevel, !!!income_abbrev), Scenario, Food,
      CostPerCapitaDay = PopulationWeightedFoodCostPerCapitaDay
    )
) %>%
  left_join(food_lookup, by = "Food")

if (anyNA(combined_food$FoodGroup)) stop("At least one food lacks a FoodGroup mapping.")

group_cost <- combined_food %>%
  group_by(Geography, Scenario, FoodGroup) %>%
  summarise(CostPerCapitaDay = sum(CostPerCapitaDay), .groups = "drop") %>%
  group_by(Geography, Scenario) %>%
  mutate(SharePct = 100 * CostPerCapitaDay / sum(CostPerCapitaDay)) %>%
  ungroup() %>%
  mutate(
    Geography = factor(Geography, levels = geo_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = c("Current", "EAT"), ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_group_levels, ordered = TRUE)
  )

p_b <- ggplot(group_cost, aes(x = Scenario, y = CostPerCapitaDay, fill = FoodGroup)) +
  geom_col(width = 0.68, position = position_stack(reverse = TRUE)) +
  facet_grid(. ~ Geography, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = food_palette, drop = FALSE) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.035))
  ) +
  labs(
    tag = "b",
    title = "Composition of per-capita food cost",
    x = NULL, y = "Food cost (US$ person−1 day−1)"
  ) +
  guides(fill = guide_legend(title = NULL, nrow = 2, byrow = TRUE)) +
  theme_nature() +
  theme(
    axis.text.x = element_text(size = 12, colour = "#000000", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 12, colour = "#000000"),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "#E2E2E2"),
    panel.spacing.x = grid::unit(1.3, "mm"),
    legend.position = "bottom",
    legend.text = element_text(size = 12, colour = "#000000"),
    legend.key.size = grid::unit(4.2, "mm"),
    legend.spacing.x = grid::unit(1.5, "mm")
  )

change_group <- group_cost %>%
  select(Geography, Scenario, FoodGroup, CostPerCapitaDay) %>%
  pivot_wider(names_from = Scenario, values_from = CostPerCapitaDay) %>%
  mutate(
    Difference = EAT - Current,
    Geography = factor(as.character(Geography), levels = rev(geo_levels), ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_group_levels, ordered = TRUE)
  )

change_labels <- change_group %>%
  group_by(Geography) %>%
  summarise(
    TotalDifference = sum(Difference),
    PositiveExtent = sum(pmax(Difference, 0)),
    .groups = "drop"
  ) %>%
  mutate(Label = sprintf("%+.2f", TotalDifference))

# Reconcile group contributions with the summary sheets before plotting.
expected_change <- bind_rows(
  global_summary %>% transmute(Geography = "W", Expected = Difference),
  income_summary %>%
    transmute(Geography = recode(IncomeLevel, !!!income_abbrev), Expected = Difference)
) %>%
  mutate(Geography = as.character(Geography))
actual_change <- change_labels %>%
  transmute(Geography = as.character(Geography), Actual = TotalDifference)
reconciliation <- left_join(expected_change, actual_change, by = "Geography")
if (any(abs(reconciliation$Expected - reconciliation$Actual) > 1e-9)) {
  stop("Food-group differences do not reconcile with income/global summaries.")
}

band_df <- tibble(
  y = seq_along(levels(change_group$Geography)),
  ymin = y - 0.5,
  ymax = y + 0.5
) %>%
  filter(y %% 2 == 1)

p_c <- ggplot(change_group, aes(x = Difference, y = Geography, fill = FoodGroup)) +
  geom_rect(
    data = band_df,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#F2F2F2", colour = NA
  ) +
  geom_vline(xintercept = 0, linewidth = 0.45, linetype = "22", colour = "#A92838") +
  geom_col(width = 0.62, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = change_labels,
    aes(x = PositiveExtent, y = Geography, label = Label),
    inherit.aes = FALSE, hjust = -0.22, size = 10 / ggplot2::.pt,
    family = "Helvetica", fontface = "bold", colour = "#000000"
  ) +
  scale_fill_manual(values = food_palette, drop = FALSE) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.5),
    expand = expansion(mult = c(0.04, 0.30))
  ) +
  labs(
    tag = "c",
    title = "Food-group contributions to cost change",
    x = "Change in per-capita daily food cost (US$)", y = NULL
  ) +
  guides(fill = "none") +
  theme_nature() +
  theme(
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.text.y = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.title = element_text(size = 12, colour = "#000000"),
    panel.grid.major.x = element_line(linewidth = 0.25, colour = "#D5D5D5"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(2, 9, 2, 2)
  )

legend_gtable <- ggplotGrob(p_b + theme(legend.position = "bottom"))
legend_index <- which(legend_gtable$layout$name == "guide-box-bottom")
if (length(legend_index) != 1L) stop("Could not resolve the bottom food-group legend.")
food_legend <- legend_gtable$grobs[[legend_index]]
p_b_no_legend <- p_b + theme(legend.position = "none")
legend_panel <- wrap_elements(full = food_legend)

lower_row <- (p_b_no_legend | p_c) +
  plot_layout(widths = c(1.04, 1), guides = "keep")

fig_s4 <- panel_a / lower_row / legend_panel +
  plot_layout(heights = c(1.42, 1, 0.18))

width_mm <- 183
height_mm <- 190
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(
  paste0(output_prefix, ".svg"),
  width = width_in, height = height_in, bg = "white"
)
print(fig_s4)
dev.off()

cat("Created:", paste0(output_prefix, ".svg"), "\n")
cat("Countries:", n_distinct(country_summary$ISO3), "\n")
cat("Foods mapped:", nrow(food_lookup), "\n")
cat("Country change range (%):", paste(round(range(country_summary$ChangePct), 2), collapse = " to "), "\n")
cat("Global per-capita daily cost change:", round(global_summary$Difference, 4), "\n")
