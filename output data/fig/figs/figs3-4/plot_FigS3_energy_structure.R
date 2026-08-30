#!/usr/bin/env Rscript

# Figure S3 contract
# Core conclusion: the EAT-Lancet dietary target changes both total dietary
# energy intake and the calorie composition of diets unevenly across countries
# and income groups.
# Evidence chain: (a) national total energy intake under Current and EAT;
# (b) population-weighted energy intake stacked by eight food groups for World
# and four income groups; (c) signed food-group changes and their mean absolute
# relative change (dietary structure change).

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

required_packages <- c("svglite", "jsonlite")
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
  normalizePath("plot_FigS3_energy_structure.R", mustWork = TRUE)
}
script_dir <- dirname(script_path)

trailing_args <- commandArgs(trailingOnly = TRUE)
json_arg <- trailing_args[grepl("^--data-json=", trailing_args)]
data_json_path <- if (length(json_arg) == 1L) {
  normalizePath(sub("^--data-json=", "", json_arg), mustWork = FALSE)
} else {
  NULL
}

demand_path <- file.path(script_dir, "sensitivity-EAT-Lancet-demand.xlsx")
calorie_path <- file.path(script_dir, "calories.csv")
population_path <- file.path(script_dir, "population_long.xlsx")
income_path <- file.path(script_dir, "Income-level.xlsx")
world_path <- file.path(dirname(script_dir), "world.geojson")
output_path <- file.path(script_dir, "FigS3_energy_structure.svg")

for (path in c(demand_path, calorie_path, population_path, income_path, world_path)) {
  if (!file.exists(path)) stop("Missing input file: ", path)
}

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
}))

if (anyDuplicated(food_lookup$Food)) {
  stop("A food item is assigned to more than one food group.")
}

current_wide <- read_excel(demand_path, sheet = "Demand_country")
eat_wide <- read_excel(demand_path, sheet = "Demand_country_EAT")
calorie_wide <- read.csv(calorie_path, check.names = FALSE)
population_wide <- read_excel(population_path)
income <- read_excel(income_path)

names(current_wide)[1] <- "ISO3"
names(eat_wide)[1] <- "ISO3"
names(population_wide)[1] <- "ISO3"
names(income)[1:2] <- c("ISO3", "IncomeLevel")

foods <- setdiff(names(current_wide), "ISO3")
if (!identical(foods, setdiff(names(eat_wide), "ISO3"))) {
  stop("Current and EAT demand food columns do not match.")
}
if (!setequal(foods, food_lookup$Food)) {
  stop("The food-group definitions do not exactly cover all demand foods.")
}
if (!setequal(foods, setdiff(names(calorie_wide), "ISO3"))) {
  stop("Demand and calorie-factor food columns do not match.")
}
if (anyDuplicated(current_wide$ISO3) || anyDuplicated(eat_wide$ISO3)) {
  stop("Duplicate country codes were found in the demand workbook.")
}

country_iso3 <- sort(current_wide$ISO3)
if (!identical(country_iso3, sort(eat_wide$ISO3))) {
  stop("Current and EAT country coverage does not match.")
}
if (length(setdiff(country_iso3, calorie_wide$ISO3)) > 0L) {
  stop("At least one demand country lacks calorie factors.")
}
if (!setequal(country_iso3, unique(population_wide$ISO3))) {
  stop("Demand and population country coverage does not match.")
}
if (!setequal(country_iso3, income$ISO3)) {
  stop("Demand and income-group country coverage does not match.")
}

calorie_extra_iso3 <- setdiff(calorie_wide$ISO3, country_iso3)
calorie_wide <- calorie_wide %>% filter(ISO3 %in% country_iso3)

population_cols <- setdiff(names(population_wide), c("ISO3", "Sex"))
population_country <- population_wide %>%
  mutate(PopulationRow = rowSums(across(all_of(population_cols)), na.rm = FALSE)) %>%
  group_by(ISO3) %>%
  summarise(Population = sum(PopulationRow), .groups = "drop")

if (anyNA(population_country$Population) || any(population_country$Population <= 0)) {
  stop("Country population must be complete and positive.")
}

demand_long <- bind_rows(
  current_wide %>%
    pivot_longer(all_of(foods), names_to = "Food", values_to = "DemandGram") %>%
    mutate(Scenario = "Current"),
  eat_wide %>%
    pivot_longer(all_of(foods), names_to = "Food", values_to = "DemandGram") %>%
    mutate(Scenario = "EAT")
) %>%
  select(Scenario, ISO3, Food, DemandGram)

calorie_long <- calorie_wide %>%
  pivot_longer(all_of(foods), names_to = "Food", values_to = "KcalPerGram")

country_food <- demand_long %>%
  left_join(calorie_long, by = c("ISO3", "Food")) %>%
  left_join(food_lookup, by = "Food") %>%
  mutate(KcalPerCapitaDay = DemandGram * KcalPerGram)

if (anyNA(country_food[c("DemandGram", "KcalPerGram", "FoodGroup")])) {
  stop("Missing value after joining demand, calorie factors and food groups.")
}
if (any(country_food$DemandGram < 0) || any(country_food$KcalPerGram < 0)) {
  stop("Demand and calorie factors must be non-negative.")
}

country_group <- country_food %>%
  group_by(ISO3, Scenario, FoodGroup) %>%
  summarise(KcalPerCapitaDay = sum(KcalPerCapitaDay), .groups = "drop") %>%
  mutate(
    FoodGroup = factor(FoodGroup, levels = food_group_levels, ordered = TRUE)
  )

if (nrow(country_group) != length(country_iso3) * 2L * length(food_group_levels)) {
  stop("Country-scenario-food-group table is incomplete.")
}

country_group_change <- country_group %>%
  pivot_wider(names_from = Scenario, values_from = KcalPerCapitaDay) %>%
  mutate(
    DifferenceKcal = EAT - Current,
    ChangePct = 100 * DifferenceKcal / Current,
    AbsoluteChangePct = abs(ChangePct)
  )

if (any(country_group_change$Current <= 0)) {
  stop("The structure-change denominator is zero or negative for at least one country-food group.")
}

country_structure <- country_group_change %>%
  group_by(ISO3) %>%
  summarise(
    StructureDeviationPct = mean(AbsoluteChangePct),
    FoodGroupCount = n(),
    .groups = "drop"
  )

if (any(country_structure$FoodGroupCount != 8L)) {
  stop("Each country must have exactly eight food groups in the structure metric.")
}

country_total_long <- country_group %>%
  group_by(ISO3, Scenario) %>%
  summarise(TotalKcalPerCapitaDay = sum(KcalPerCapitaDay), .groups = "drop")

country_panel_a <- country_total_long %>%
  pivot_wider(names_from = Scenario, values_from = TotalKcalPerCapitaDay) %>%
  left_join(population_country, by = "ISO3") %>%
  left_join(income, by = "ISO3") %>%
  left_join(country_structure %>% select(-FoodGroupCount), by = "ISO3") %>%
  mutate(
    DifferenceKcal = EAT - Current,
    ChangePct = 100 * DifferenceKcal / Current
  ) %>%
  select(
    ISO3, IncomeLevel, Population, CurrentKcalPerCapitaDay = Current,
    EATKcalPerCapitaDay = EAT, DifferenceKcal, ChangePct,
    StructureDeviationPct
  ) %>%
  arrange(ISO3)

country_group_weighted <- country_group %>%
  left_join(population_country, by = "ISO3") %>%
  left_join(income, by = "ISO3")

summarise_weighted_group <- function(data, group_cols) {
  data %>%
    group_by(across(all_of(group_cols)), Scenario, FoodGroup) %>%
    summarise(
      WeightedNumerator = sum(KcalPerCapitaDay * Population),
      Population = sum(Population),
      Countries = n_distinct(ISO3),
      .groups = "drop"
    ) %>%
    mutate(KcalPerCapitaDay = WeightedNumerator / Population) %>%
    select(-WeightedNumerator)
}

income_group_long <- summarise_weighted_group(country_group_weighted, "IncomeLevel") %>%
  mutate(
    Geography = recode(
      IncomeLevel,
      "High income" = "H",
      "Upper-middle income" = "UM",
      "Lower middle income" = "LM",
      "Low income" = "L"
    )
  ) %>%
  select(Geography, GeographyName = IncomeLevel, Scenario, FoodGroup,
         Countries, Population, KcalPerCapitaDay)

world_group_long <- country_group_weighted %>%
  mutate(Geography = "W", GeographyName = "World") %>%
  summarise_weighted_group(c("Geography", "GeographyName")) %>%
  select(Geography, GeographyName, Scenario, FoodGroup,
         Countries, Population, KcalPerCapitaDay)

geo_levels <- c("W", "H", "UM", "LM", "L")
regional_group_long <- bind_rows(world_group_long, income_group_long) %>%
  mutate(
    Geography = factor(Geography, levels = geo_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = c("Current", "EAT"), ordered = TRUE),
    FoodGroup = factor(FoodGroup, levels = food_group_levels, ordered = TRUE)
  )

regional_totals <- regional_group_long %>%
  group_by(Geography, GeographyName, Scenario) %>%
  summarise(TotalKcalPerCapitaDay = sum(KcalPerCapitaDay), .groups = "drop")

panel_b_data <- regional_group_long %>%
  left_join(
    regional_totals,
    by = c("Geography", "GeographyName", "Scenario")
  ) %>%
  arrange(Geography, Scenario, FoodGroup)

panel_c_data <- regional_group_long %>%
  select(Geography, GeographyName, Scenario, FoodGroup, KcalPerCapitaDay) %>%
  pivot_wider(names_from = Scenario, values_from = KcalPerCapitaDay) %>%
  mutate(
    DifferenceKcal = EAT - Current,
    ChangePct = 100 * DifferenceKcal / Current,
    AbsoluteChangePct = abs(ChangePct)
  ) %>%
  group_by(Geography, GeographyName) %>%
  mutate(StructureDeviationPct = mean(AbsoluteChangePct)) %>%
  ungroup() %>%
  arrange(Geography, FoodGroup)

# Reconcile panel-b totals with a direct population-weighted country-total calculation.
direct_regional_totals <- bind_rows(
  country_total_long %>%
    left_join(population_country, by = "ISO3") %>%
    mutate(Geography = "W") %>%
    group_by(Geography, Scenario) %>%
    summarise(
      DirectTotal = sum(TotalKcalPerCapitaDay * Population) / sum(Population),
      .groups = "drop"
    ),
  country_total_long %>%
    left_join(population_country, by = "ISO3") %>%
    left_join(income, by = "ISO3") %>%
    mutate(
      Geography = recode(
        IncomeLevel,
        "High income" = "H",
        "Upper-middle income" = "UM",
        "Lower middle income" = "LM",
        "Low income" = "L"
      )
    ) %>%
    group_by(Geography, Scenario) %>%
    summarise(
      DirectTotal = sum(TotalKcalPerCapitaDay * Population) / sum(Population),
      .groups = "drop"
    )
)

total_reconciliation <- regional_totals %>%
  mutate(Geography = as.character(Geography), Scenario = as.character(Scenario)) %>%
  left_join(direct_regional_totals, by = c("Geography", "Scenario")) %>%
  mutate(Error = TotalKcalPerCapitaDay - DirectTotal)

if (max(abs(total_reconciliation$Error)) > 1e-9) {
  stop("Food-group totals do not reconcile with direct weighted energy totals.")
}

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

theme_nature <- function(base_size = 12) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "#000000"),
      axis.ticks = element_line(linewidth = 0.35, colour = "#000000"),
      axis.text = element_text(colour = "#000000"),
      axis.title = element_text(colour = "#000000"),
      plot.title = element_text(size = base_size + 0.5, face = "bold", colour = "#000000"),
      plot.tag = element_text(size = 12, face = "bold", colour = "#000000"),
      strip.text = element_text(size = base_size, face = "bold", colour = "#000000"),
      strip.background = element_blank(),
      legend.title = element_text(size = base_size, colour = "#000000"),
      legend.text = element_text(size = base_size, colour = "#000000"),
      panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Panel a: Current and EAT calorie maps plus country-level dietary structure change.
previous_s2 <- sf_use_s2()
sf_use_s2(FALSE)
world <- st_read(world_path, quiet = TRUE) %>%
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) %>%
  mutate(ISO3 = recode(as.character(SOC), ROM = "ROU", TMP = "TLS")) %>%
  st_make_valid() %>%
  suppressWarnings(st_break_antimeridian(lon_0 = 0, tol = 1e-4))
sf_use_s2(previous_s2)

unmatched_iso3 <- setdiff(country_iso3, world$ISO3)
if (length(unmatched_iso3) > 0L) {
  stop("Country code(s) missing from the map: ", paste(unmatched_iso3, collapse = ", "))
}

scenario_levels <- c("Current", "EAT")
world_by_scenario <- world[rep(seq_len(nrow(world)), times = 2L), ]
world_by_scenario$Scenario <- rep(scenario_levels, each = nrow(world))
map_values <- country_total_long %>%
  mutate(Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE))
map_data <- world_by_scenario %>%
  left_join(map_values, by = c("ISO3", "Scenario")) %>%
  mutate(Scenario = factor(Scenario, levels = scenario_levels, ordered = TRUE))

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
calorie_limits <- c(
  floor(min(country_total_long$TotalKcalPerCapitaDay) / 100) * 100,
  3000
)
calorie_breaks <- c(2000, 2500, 3000)
calorie_breaks <- calorie_breaks[
  calorie_breaks >= calorie_limits[1] & calorie_breaks <= calorie_limits[2]
]

p_a_energy <- ggplot(map_data) +
  geom_sf(aes(fill = TotalKcalPerCapitaDay), colour = "white", linewidth = 0.055) +
  facet_wrap(vars(Scenario), nrow = 1) +
  scale_fill_gradientn(
    colours = c("#E8EFF3", "#B9D1D4", "#70A6A2", "#2F6F6A", "#173F4A"),
    limits = calorie_limits,
    breaks = calorie_breaks,
    labels = label_number(accuracy = 1, big.mark = ","),
    oob = scales::squish,
    na.value = "#E3E3E3",
    guide = guide_colourbar(
      title = "Energy intake (kcal person−1 day−1)",
      title.position = "top", title.hjust = 0.5,
      barwidth = grid::unit(52, "mm"), barheight = grid::unit(3, "mm"),
      ticks.colour = "#000000", frame.colour = "#8A8A8A"
    )
  ) +
  coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
  labs(tag = "a") +
  theme_void(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", colour = "#000000"),
    strip.text = element_text(size = 10, face = "bold", colour = "#000000"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, colour = "#000000"),
    legend.text = element_text(size = 12, colour = "#000000"),
    plot.tag = element_text(size = 12, face = "bold", colour = "#000000"),
    plot.tag.position = c(0.002, 0.998),
    panel.spacing.x = grid::unit(0.2, "mm"),
    plot.margin = margin(0, -7, 0, -7)
  )

structure_map_data <- world %>%
  left_join(
    country_structure %>% select(ISO3, StructureDeviationPct),
    by = "ISO3"
  )

# Values above 400% are assigned the same terminal colour so that a few extreme
# countries do not compress the contrast across the rest of the map.
p_a_structure <- ggplot(structure_map_data) +
  geom_sf(aes(fill = StructureDeviationPct), colour = "white", linewidth = 0.055) +
  scale_fill_gradientn(
    colours = c("#F4F1EA", "#E8C97A", "#D47A55", "#9E2F3F", "#5B1735"),
    limits = c(0, 400),
    breaks = c(0, 100, 200, 300, 400),
    labels = label_percent(scale = 1, accuracy = 1),
    oob = scales::squish,
    na.value = "#E3E3E3",
    guide = guide_colourbar(
      title = "Dietary structure change",
      title.position = "top", title.hjust = 0.5,
      barwidth = grid::unit(44, "mm"), barheight = grid::unit(3, "mm"),
      ticks.colour = "#000000", frame.colour = "#8A8A8A"
    )
  ) +
  coord_sf(crs = moll_crs, datum = NA, expand = FALSE) +
  facet_wrap(~"Structure change") +
  theme_void(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", colour = "#000000"),
    strip.text = element_text(size = 10, face = "bold", colour = "#000000"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, colour = "#000000"),
    legend.text = element_text(size = 12, colour = "#000000"),
    plot.margin = margin(0, 0, 0, -7)
  )

p_a <- p_a_energy + p_a_structure +
  plot_layout(widths = c(2, 1))

# Panel b: absolute calorie stacks for World and income groups.
bar_totals <- regional_totals %>%
  mutate(
    Geography = factor(Geography, levels = geo_levels, ordered = TRUE),
    Scenario = factor(Scenario, levels = c("Current", "EAT"), ordered = TRUE)
  )

p_b <- ggplot(panel_b_data, aes(x = Scenario, y = KcalPerCapitaDay, fill = FoodGroup)) +
  geom_col(width = 0.68, position = position_stack(reverse = TRUE)) +
  geom_text(
    data = bar_totals,
    aes(x = Scenario, y = TotalKcalPerCapitaDay, label = round(TotalKcalPerCapitaDay)),
    inherit.aes = FALSE, vjust = -0.35, size = 10 / ggplot2::.pt,
    family = "Arial", fontface = "bold", colour = "#000000"
  ) +
  facet_grid(. ~ Geography, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = food_palette, drop = FALSE) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ","),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    tag = "b", x = NULL,
    y = "Energy intake (kcal person−1 day−1)",
    fill = NULL
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_nature(12) +
  theme(
    text = element_text(family = "Arial", colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.text.y = element_text(size = 12, colour = "#000000"),
    axis.title = element_text(size = 12, colour = "#000000"),
    strip.text = element_text(size = 10, face = "bold", colour = "#000000"),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "#E0E0E0"),
    panel.spacing.x = grid::unit(1.8, "mm"),
    legend.position = "bottom",
    legend.key.size = grid::unit(4.2, "mm"),
    legend.title = element_text(size = 12, colour = "#000000"),
    legend.text = element_text(size = 12, colour = "#000000"),
    plot.tag.position = c(0.075, 0.998)
  )

# Panel c: signed food-group change from the Current baseline (0%).
panel_c_plot <- panel_c_data %>%
  mutate(
    Geography = factor(as.character(Geography), levels = geo_levels, ordered = TRUE),
    GeographyDisplay = recode(
      as.character(Geography),
      W = "World", H = "High", UM = "Upper-middle",
      LM = "Lower-middle", L = "Low"
    ),
    GeographyDisplay = factor(
      GeographyDisplay,
      levels = c("World", "High", "Upper-middle", "Lower-middle", "Low"),
      ordered = TRUE
    ),
    FoodGroup = factor(FoodGroup, levels = rev(food_group_levels), ordered = TRUE),
    Label = paste0(if_else(ChangePct > 0, "+", ""), round(ChangePct), "%"),
    LabelHjust = if_else(ChangePct >= 0, -0.16, 1.16)
  )

p_c <- ggplot(panel_c_plot, aes(y = FoodGroup)) +
  geom_vline(xintercept = 0, linewidth = 0.38, colour = "#3F3F3F") +
  geom_segment(
    aes(x = 0, xend = ChangePct, yend = FoodGroup, colour = FoodGroup),
    linewidth = 0.75
  ) +
  geom_point(
    aes(x = 0), shape = 21, size = 1.9, stroke = 0.45,
    fill = "white", colour = "#4A4A4A"
  ) +
  geom_point(
    aes(x = ChangePct, fill = FoodGroup), shape = 22,
    size = 2.1, stroke = 0.4, colour = "#404040"
  ) +
  geom_text(
    aes(x = ChangePct, label = Label, hjust = LabelHjust),
    size = 10 / ggplot2::.pt, family = "Arial",
    fontface = "bold", colour = "#000000"
  ) +
  facet_grid(
    GeographyDisplay ~ ., scales = "free_y", space = "free_y",
    switch = "y"
  ) +
  scale_colour_manual(values = food_palette, guide = "none") +
  scale_fill_manual(values = food_palette, guide = "none") +
  scale_x_continuous(
    limits = c(-100, 300),
    breaks = c(-100, 0, 100, 200, 300),
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    tag = "c", x = "Change from current diets", y = NULL
  ) +
  theme_nature(12) +
  theme(
    text = element_text(family = "Arial", colour = "#000000"),
    axis.text.y = element_text(size = 12, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.title.x = element_text(size = 12, colour = "#000000"),
    panel.grid.major.x = element_line(linewidth = 0.25, colour = "#D8D8D8"),
    panel.grid.major.y = element_blank(),
    strip.text.y.left = element_text(
      size = 12, face = "bold", angle = 90, colour = "#000000",
      margin = margin(0, 1.5, 0, 1.5)
    ),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.spacing.y = grid::unit(0.5, "mm"),
    plot.tag.position = c(0.002, 0.998),
    plot.margin = margin(2, 10, 2, 2)
  )

# Keep the three outer panel widths identical while allowing each panel to use
# its own axis/label width. This prevents panel c's long y-axis labels from
# imposing the same left gutter on the maps and stacked bars.
fig_s3 <- free(p_a) / free(p_b) / free(p_c) +
  plot_layout(heights = c(0.55, 0.70, 1.55))

width_mm <- 183
height_mm <- 280
svglite::svglite(
  output_path,
  width = width_mm / 25.4,
  height = height_mm / 25.4,
  bg = "white"
)
print(fig_s3)
dev.off()

qa_table <- tibble(
  Check = c(
    "Demand countries",
    "Foods matched across demand, calorie factors and groups",
    "Food groups",
    "Country-food-group Current denominators <= 0",
    "Missing joined calculation values",
    "Maximum regional total reconciliation error",
    "Population total",
    "Extra calorie-factor countries excluded",
    "Structure-change formula"
  ),
  Result = c(
    length(country_iso3),
    length(foods),
    length(food_group_levels),
    sum(country_group_change$Current <= 0),
    sum(is.na(country_food$KcalPerCapitaDay)),
    format(max(abs(total_reconciliation$Error)), scientific = TRUE),
    format(sum(population_country$Population), scientific = FALSE, trim = TRUE),
    if_else(length(calorie_extra_iso3) == 0L, "None", paste(calorie_extra_iso3, collapse = ", ")),
    "mean(abs(EAT group kcal - Current group kcal) / Current group kcal)"
  )
)

if (!is.null(data_json_path)) {
  dir.create(dirname(data_json_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      metadata = list(
        title = "Figure S3 plotting data",
        demand_file = basename(demand_path),
        calorie_file = basename(calorie_path),
        population_file = basename(population_path),
        income_file = basename(income_path),
        country_count = length(country_iso3),
        food_count = length(foods),
        food_group_count = length(food_group_levels),
        formula_total_kcal = "Demand (g person-1 day-1) * calorie factor (kcal g-1)",
        formula_structure = "mean(abs(EAT group kcal - Current group kcal) / Current group kcal)"
      ),
      figS3a = country_panel_a,
      figS3b = panel_b_data %>%
        mutate(across(c(Geography, Scenario, FoodGroup), as.character)),
      figS3c = panel_c_data %>%
        mutate(across(c(Geography, FoodGroup), as.character)),
      qa = qa_table
    ),
    path = data_json_path,
    dataframe = "rows",
    auto_unbox = TRUE,
    digits = 15,
    pretty = TRUE,
    na = "null"
  )
  cat("Created plotting-data JSON:", data_json_path, "\n")
}

cat("Created figure:", output_path, "\n")
cat("Countries:", length(country_iso3), "\n")
cat("Foods:", length(foods), "\n")
cat("Food groups:", length(food_group_levels), "\n")
cat("Calorie-factor-only countries excluded:",
    if_else(length(calorie_extra_iso3) == 0L, "None", paste(calorie_extra_iso3, collapse = ", ")), "\n")
cat("Country structure-change range (%):",
    paste(round(range(country_panel_a$StructureDeviationPct), 1), collapse = " to "), "\n")
