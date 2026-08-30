suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(sf)
  library(grid)
})

script_arg <- grep(
  "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE
)
data_dir <- if (length(script_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  normalizePath(getwd())
}

fig2_path <- file.path(data_dir, "00fig2.xlsx")
population_path <- file.path(data_dir, "population_long.xlsx")
iso_groups_path <- file.path(data_dir, "ISO groups.xlsx")
world_path <- file.path(data_dir, "world.geojson")
output_path <- file.path(data_dir, "Fig2new.svg")

stopifnot(
  file.exists(fig2_path),
  file.exists(population_path),
  file.exists(iso_groups_path),
  file.exists(world_path)
)

font_family <- "Arial"

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

region_levels <- c(
  "World", "High income", "Upper-middle income",
  "Lower middle income", "Low income"
)

income_levels <- region_levels[-1]
sex_levels <- c("MLE", "FML")

food_levels <- c(
  "Seafood", "Dairy & eggs", "Meat", "Sugar & oil",
  "Staple foods", "Legumes & nuts", "Fruits", "Vegetable"
)

food_colors <- c(
  "Seafood" = "#5790b7",
  "Dairy & eggs" = "#bd9cb8",
  "Meat" = "#b85648",
  "Sugar & oil" = "#c98484",
  "Staple foods" = "#cb9b4c",
  "Legumes & nuts" = "#268673",
  "Fruits" = "#c1d6ae",
  "Vegetable" = "#8db5a0"
)

age_colors <- setNames(
  hcl.colors(length(age_levels), palette = "Spectral", rev = TRUE),
  age_levels
)

minus_sign <- "\u2212"

fmt_signed <- function(x, digits = 1, suffix = "") {
  paste0(
    ifelse(x > 0, "+", ifelse(x < 0, minus_sign, "")),
    formatC(abs(x), format = "f", digits = digits),
    suffix
  )
}

# ============================================================
# 1. Read source data
# ============================================================

fig2a <- read_excel(fig2_path, sheet = "fig2a")
fig2b <- read_excel(fig2_path, sheet = "fig2b")
fig2c <- read_excel(fig2_path, sheet = "fig2c")
fig2d <- read_excel(fig2_path, sheet = "fig2d")
population_wide <- read_excel(population_path, sheet = 1)
iso_groups <- read_excel(iso_groups_path, sheet = 1)

# ============================================================
# 2. Panel a: country changes + Mollweide projection
# ============================================================

country_change <- fig2a %>%
  select(ISO3, Scenario, Value) %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  mutate(
    ISO3 = as.character(ISO3),
    ChangePct = (`Optimized` - `Current`) / `Current` * 100
  )

country_income_lookup <- iso_groups %>%
  select(ISO3, all_of(income_levels)) %>%
  pivot_longer(
    cols = all_of(income_levels),
    names_to = "region",
    values_to = "Member"
  ) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Member = toupper(as.character(Member))
  ) %>%
  filter(Member %in% c("Y", "YES", "TRUE", "1")) %>%
  distinct(ISO3, region)

income_short_labels <- c(
  "World" = "W",
  "High income" = "H",
  "Upper-middle income" = "UM",
  "Lower middle income" = "LM",
  "Low income" = "L"
)

income_direction_counts <- country_change %>%
  inner_join(country_income_lookup, by = "ISO3") %>%
  mutate(
    Direction = case_when(
      ChangePct < 0 ~ "Decrease",
      ChangePct > 0 ~ "Increase",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Direction)) %>%
  count(region, Direction, name = "CountryCount")

world_direction_counts <- country_change %>%
  mutate(
    region = "World",
    Direction = case_when(
      ChangePct < 0 ~ "Decrease",
      ChangePct > 0 ~ "Increase",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Direction)) %>%
  count(region, Direction, name = "CountryCount")

country_direction_counts <- bind_rows(
  world_direction_counts,
  income_direction_counts
) %>%
  complete(
    region = region_levels,
    Direction = c("Decrease", "Increase"),
    fill = list(CountryCount = 0)
  ) %>%
  mutate(
    Direction = factor(Direction, levels = c("Decrease", "Increase")),
    CountLabel = if_else(CountryCount > 0, as.character(CountryCount), "")
  )

moll_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

world_map <- sf::st_read(world_path, quiet = TRUE) %>%
  filter(!grepl("ATA", SOC, ignore.case = TRUE)) %>%
  mutate(ISO3 = as.character(SOC)) %>%
  left_join(
    country_change %>% select(ISO3, ChangePct),
    by = "ISO3"
  ) %>%
  sf::st_transform(crs = moll_crs)

map_bbox <- sf::st_bbox(world_map)
map_xlim <- unname(map_bbox[c("xmin", "xmax")])
map_ylim <- unname(map_bbox[c("ymin", "ymax")])

map_limit <- 60

p_a_map <- ggplot(world_map) +
  geom_sf(
    aes(fill = ChangePct),
    color = "white",
    linewidth = 0,
    na.rm = FALSE
  ) +
  scale_fill_gradient2(
    low = "#0f3b6b",
    mid = "#f2f3f4",
    high = "#891d2e",
    midpoint = 0,
    limits = c(-map_limit, map_limit),
    breaks = seq(-60, 60, by = 20),
    oob = squish,
    na.value = "grey84",
    name = "Change in food cost (%)"
  ) +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  labs(title = NULL, tag = "b") +
  theme_void(base_family = font_family, base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 3)),
    plot.tag = element_text(
      face = "bold", size = 12, hjust = 0, vjust = 1
    ),
    plot.tag.position = c(0.01, 0.99),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(13, "mm"),
    legend.key.height = unit(2.8, "mm"),
    legend.title = element_text(size = 12, margin = margin(b = 2)),
    legend.text = element_text(size = 12),
    legend.margin = margin(0, 0, 6, 0),
    legend.box.margin = margin(0),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 12, -3)
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(90, "mm"),
    barheight = unit(3, "mm")
  ))

make_pie <- function(region_name) {
  pie_data <- country_direction_counts %>%
    filter(region == region_name)

  ggplot(pie_data, aes(x = 2, y = CountryCount, fill = Direction)) +
    geom_col(width = 0.9, linewidth = 0) +
    geom_text(
      aes(label = CountLabel),
      position = position_stack(vjust = 0.5),
      family = font_family,
      fontface = "bold",
      size = 8 / ggplot2::.pt,
      color = "white"
    ) +
    annotate(
      "point", x = 0.5, y = 0,
      shape = 21, size = 7.2, stroke = 0, fill = "white"
    ) +
    annotate(
      "text", x = 0.5, y = 0,
      label = income_short_labels[[region_name]],
      family = font_family,
      fontface = "bold",
      size = 10.5 / ggplot2::.pt
    ) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(
      values = c("Decrease" = "#2362a2", "Increase" = "#ba3d4c"),
      drop = FALSE
    ) +
    theme_void(base_family = font_family) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
}

pie_boxes <- tibble::tribble(
  ~region,                  ~left, ~bottom, ~right, ~top,
  "World",                  0.035,   0.205,  0.205, 0.555,
  "High income",            0.270,   0.495,  0.440, 0.845,
  "Upper-middle income",    0.335,   0.010,  0.505, 0.350,
  "Lower middle income",    0.650,   0.145,  0.820, 0.495,
  "Low income",             0.855,   0.495,  1.025, 0.845
)

direction_key <- tibble(
  Direction = factor(
    c("Decrease", "Increase"),
    levels = c("Decrease", "Increase")
  ),
  y = c(2, 1),
  Label = c("Cost decrease", "Cost increase")
) %>%
  ggplot(aes(x = 1, y = y, fill = Direction)) +
  geom_tile(width = 0.55, height = 0.62) +
  geom_text(
    aes(x = 1.42, label = Label),
    hjust = 0,
    family = font_family,
    size = 9 / ggplot2::.pt
  ) +
  scale_fill_manual(
    values = c("Decrease" = "#2362a2", "Increase" = "#ba3d4c")
  ) +
  coord_cartesian(xlim = c(0.7, 4.3), ylim = c(0.5, 2.5), clip = "off") +
  theme_void(base_family = font_family) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

p_a <- p_a_map
for (i in seq_len(nrow(pie_boxes))) {
  box <- pie_boxes[i, ]
  p_a <- p_a + patchwork::inset_element(
    make_pie(box$region),
    left = box$left,
    bottom = box$bottom,
    right = box$right,
    top = box$top,
    align_to = "panel",
    on_top = TRUE,
    clip = FALSE
  )
}
p_a <- p_a + patchwork::inset_element(
  direction_key,
  left = 0.13,
  bottom = 0.22,
  right = 0.41,
  top = 0.36,
  align_to = "full",
  on_top = TRUE,
  clip = FALSE
)

# ============================================================
# 3. Panel b: current versus optimized costs
# ============================================================

cost_b <- fig2b %>%
  filter(FoodGroup == "Total", region %in% region_levels) %>%
  select(region, Scenario, Intake) %>%
  pivot_wider(names_from = Scenario, values_from = Intake) %>%
  rename(
    Current = `Current diets`,
    Optimized = `Optimized diets`
  ) %>%
  mutate(
    region = factor(region, levels = rev(region_levels)),
    PercentChange = (Optimized - Current) / Current * 100,
    Gap = abs(Optimized - Current),
    MinCost = pmin(Current, Optimized),
    Direction = if_else(PercentChange < 0, "Cost decrease", "Cost increase"),
    PercentLabel = fmt_signed(PercentChange, 1, "%"),
    PercentX = (Current + Optimized) / 2
  )

point_b <- cost_b %>%
  select(region, Current, Optimized, Direction, Gap, MinCost) %>%
  pivot_longer(
    cols = c(Current, Optimized),
    names_to = "Diet",
    values_to = "Cost"
  ) %>%
  mutate(
    LabelX = if_else(Cost == MinCost, Cost - 0.20, Cost + 0.20),
    LabelHjust = if_else(Cost == MinCost, 1, 0)
  )

b_xlim <- c(1.85, 11.72)

background_b <- cost_b %>%
  filter(as.integer(region) %% 2 == 1)

p_b <- ggplot(cost_b, aes(y = region)) +
  geom_tile(
    data = background_b,
    aes(x = mean(b_xlim), y = region),
    inherit.aes = FALSE,
    width = diff(b_xlim) * 0.998,
    height = 0.98,
    fill = "#f2f1f0"
  ) +
  geom_segment(
    aes(x = Current, xend = Optimized, yend = region),
    color = "grey55",
    linewidth = 0.65
  ) +
  geom_point(
    data = point_b %>% filter(Diet == "Current"),
    aes(x = Cost, color = "Current diets"),
    size = 3.2
  ) +
  geom_point(
    data = point_b %>% filter(Diet == "Optimized"),
    aes(x = Cost, color = Direction),
    size = 3.4
  ) +
  geom_text(
    data = point_b,
    aes(
      x = LabelX,
      label = formatC(Cost, format = "f", digits = 2),
      hjust = LabelHjust,
      vjust = 0.5
    ),
    size = 10 / ggplot2::.pt,
    fontface = "bold",
    family = font_family
  ) +
  geom_text(
    aes(
      x = PercentX,
      y = as.numeric(region) + 0.38,
      label = PercentLabel,
      color = Direction
    ),
    hjust = 0.5,
    size = 10 / ggplot2::.pt,
    fontface = "bold",
    family = font_family,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Current diets" = "grey55",
      "Cost decrease" = "#2362a2",
      "Cost increase" = "#ba3d4c"
    ),
    breaks = c("Current diets", "Cost decrease", "Cost increase"),
    labels = c(
      "Current diets",
      "Optimized diets\n(cost decrease)",
      "Optimized diets\n(cost increase)"
    )
  ) +
  scale_x_continuous(
    limits = b_xlim,
    breaks = 3:11,
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    limits = rev(region_levels),
    labels = c(
      "Low income" = "L",
      "Lower middle income" = "LM",
      "Upper-middle income" = "UM",
      "High income" = "H",
      "World" = "W"
    ),
    expand = expansion(add = c(0.45, 0.70))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = NULL,
    x = "Food cost ($)",
    y = NULL,
    color = NULL,
    tag = "a"
  ) +
  theme_minimal(base_family = font_family, base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 8)),
    plot.tag = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey84", linewidth = 0.35),
    axis.text.x = element_text(
      family = font_family, size = 12, color = "black"
    ),
    axis.text.y = element_text(
      family = font_family,
      size = 12,
      color = "black",
      margin = margin(r = 6)
    ),
    axis.title.x = element_text(
      family = font_family, size = 12, color = "black"
    ),
    axis.line.x = element_line(color = "black", linewidth = 0.45),
    axis.ticks.x = element_line(color = "black", linewidth = 0.45),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9.5),
    legend.key.width = unit(3.5, "mm"),
    legend.spacing.x = unit(0.6, "mm"),
    legend.margin = margin(0),
    legend.box.margin = margin(0),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(1, 1, 1, 1)
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 4),
    nrow = 1,
    byrow = TRUE,
    title.position = "top"
  ))

# ============================================================
# 4. Panel c: food-group contributions to absolute cost change
# ============================================================

food_contribution <- fig2c %>%
  filter(FoodGroup %in% food_levels, region %in% region_levels) %>%
  select(region, FoodGroup, Scenario, Intake) %>%
  pivot_wider(names_from = Scenario, values_from = Intake) %>%
  mutate(
    Contribution = `Optimized diets` - `Current diets`,
    FoodGroup = factor(FoodGroup, levels = food_levels),
    region = factor(region, levels = rev(region_levels))
  )

food_totals <- food_contribution %>%
  group_by(region) %>%
  summarise(
    Total = sum(Contribution, na.rm = TRUE),
    PositiveExtent = sum(pmax(Contribution, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Label = fmt_signed(Total, 2),
    LabelX = PositiveExtent + 0.18
  )

c_xlim <- c(-5.65, 3.25)

background_c <- food_contribution %>%
  distinct(region) %>%
  filter(as.integer(region) %% 2 == 1)

p_c <- ggplot(
  food_contribution,
  aes(x = Contribution, y = region, fill = FoodGroup)
) +
  geom_tile(
    data = background_c,
    aes(x = mean(c_xlim), y = region),
    inherit.aes = FALSE,
    width = diff(c_xlim) * 0.998,
    height = 0.98,
    fill = "#f2f1f0"
  ) +
  geom_col(width = 0.62, color = "white", linewidth = 0) +
  geom_text(
    data = food_totals,
    aes(x = LabelX, y = region, label = Label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 10 / ggplot2::.pt,
    fontface = "bold",
    family = font_family
  ) +
  geom_vline(
    xintercept = 0,
    color = "#b2182b",
    linetype = "dashed",
    linewidth = 0.55
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = c(
      "Seafood", "Dairy & eggs", "Meat", "Sugar & oil",
      "Staple foods", "Legumes & nuts", "Fruits", "Vegetables"
    )
  ) +
  scale_x_continuous(
    limits = c_xlim,
    breaks = -5:3,
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    limits = rev(region_levels),
    labels = c(
      "Low income" = "L",
      "Lower middle income" = "LM",
      "Upper-middle income" = "UM",
      "High income" = "H",
      "World" = "W"
    )
  ) +
  labs(
    title = NULL,
    x = "Changes in food cost ($)",
    y = NULL,
    fill = NULL,
    tag = "c"
  ) +
  theme_minimal(base_family = font_family, base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 7)),
    plot.tag = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey84", linewidth = 0.35),
    axis.text.x = element_text(
      family = font_family, size = 12, color = "black"
    ),
    axis.text.y = element_text(
      family = font_family, size = 12, color = "black"
    ),
    axis.title.x = element_text(
      family = font_family,
      size = 12,
      color = "black",
      margin = margin(t = -4)
    ),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 12),
    legend.key.size = unit(3.2, "mm"),
    axis.line.x = element_line(color = "black", linewidth = 0.45),
    axis.ticks.x = element_line(color = "black", linewidth = 0.45),
    legend.margin = margin(-4, 0, 0, 0),
    legend.box.margin = margin(0),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(1, 1, -2, 1)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# ============================================================
# 5. Panel d: age-by-sex percentage-point contributions
# ============================================================

population_long_base <- population_wide %>%
  select(ISO3, Sex, all_of(age_levels)) %>%
  pivot_longer(
    cols = all_of(age_levels),
    names_to = "Age",
    values_to = "Population"
  ) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Population = replace_na(as.numeric(Population), 0)
  )

population_income <- population_long_base %>%
  inner_join(country_income_lookup, by = "ISO3") %>%
  group_by(region, Sex, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

population_world <- population_long_base %>%
  group_by(Sex, Age) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "World")

population_weights <- bind_rows(population_world, population_income) %>%
  group_by(region) %>%
  mutate(PopShare = Population / sum(Population, na.rm = TRUE)) %>%
  ungroup()

demographic_change <- fig2d %>%
  filter(
    region %in% region_levels,
    Sex %in% sex_levels,
    Age %in% age_levels,
    FoodGroup %in% food_levels
  ) %>%
  group_by(region, Sex, Age, Scenario) %>%
  summarise(Cost = sum(Intake, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Scenario, values_from = Cost) %>%
  mutate(UnitCostChange = `Optimized diets` - `Current diets`) %>%
  left_join(population_weights, by = c("region", "Sex", "Age"))

income_totals <- cost_b %>%
  mutate(region = as.character(region)) %>%
  select(region, Current, Optimized, PercentChange)

demographic_contribution <- demographic_change %>%
  left_join(income_totals, by = "region") %>%
  mutate(
    ContributionUSD = PopShare * UnitCostChange,
    ContributionPP = ContributionUSD / Current * 100,
    Age = factor(Age, levels = age_levels),
    Sex = factor(Sex, levels = c("FML", "MLE"), labels = c("Female", "Male"))
  )

reconciliation <- demographic_contribution %>%
  group_by(region) %>%
  summarise(
    ContributionPP = sum(ContributionPP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(income_totals, by = "region") %>%
  mutate(Difference = ContributionPP - PercentChange)

if (any(abs(reconciliation$Difference) > 0.05)) {
  stop("Panel d contributions do not reconcile to panel b within 0.05 percentage points.")
}

sex_totals <- demographic_contribution %>%
  group_by(region, Sex) %>%
  summarise(
    SexPP = sum(ContributionPP, na.rm = TRUE),
    .groups = "drop"
  )

row_layout <- crossing(
  region = region_levels,
  Sex = factor(c("Male", "Female"), levels = c("Female", "Male"))
) %>%
  mutate(
    RegionOrder = match(region, region_levels),
    SexOrder = if_else(as.character(Sex) == "Male", 1L, 2L),
    RowY = 10.28 -
      (RegionOrder - 1L) * 2.08 -
      (SexOrder - 1L) * 0.96
  ) %>%
  select(region, Sex, RowY)

heatmap_d <- demographic_contribution %>%
  left_join(row_layout, by = c("region", "Sex")) %>%
  mutate(
    AgeX = match(as.character(Age), age_levels)
  )

sex_labels_d <- row_layout %>%
  left_join(sex_totals, by = c("region", "Sex")) %>%
  mutate(
    SexLabel = as.character(Sex),
    TotalLabel = fmt_signed(SexPP, 1, "%"),
    TotalColor = if_else(SexPP < 0, "#255b8e", "#b33b3b")
  )

group_positions_d <- row_layout %>%
  group_by(region) %>%
  summarise(GroupY = mean(RowY), .groups = "drop")

group_labels_d <- income_totals %>%
  left_join(group_positions_d, by = "region") %>%
  mutate(
    GroupLabel = recode(
      region,
      "World" = "W",
      "High income" = "H",
      "Upper-middle income" = "UM",
      "Lower middle income" = "LM",
      "Low income" = "L"
    )
  )

d_fill_limit <- ceiling(
  max(abs(heatmap_d$ContributionPP), na.rm = TRUE) * 2
) / 2

p_d <- ggplot(
  heatmap_d,
  aes(x = AgeX, y = RowY, fill = ContributionPP)
) +
  geom_tile(
    width = 0.98,
    height = 0.84,
    color = "white",
    linewidth = 0.30
  ) +
  geom_text(
    data = sex_labels_d,
    aes(x = 0.28, y = RowY, label = SexLabel),
    inherit.aes = FALSE,
    hjust = 1,
    size = 12 / ggplot2::.pt,
    family = font_family
  ) +
  geom_text(
    data = group_labels_d,
    aes(x = -4.15, y = GroupY, label = GroupLabel),
    inherit.aes = FALSE,
    hjust = 1,
    size = 12 / ggplot2::.pt,
    family = font_family
  ) +
  geom_text(
    data = sex_labels_d,
    aes(
      x = 18.0,
      y = RowY,
      label = TotalLabel,
      color = TotalColor
    ),
    inherit.aes = FALSE,
    hjust = 0,
    fontface = "bold",
    size = 10 / ggplot2::.pt,
    family = font_family,
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-d_fill_limit, d_fill_limit),
    breaks = seq(-4, 4, by = 2),
    oob = squish,
    name = "Contribution (%)"
  ) +
  scale_x_continuous(
    breaks = seq_along(age_levels),
    labels = age_levels,
    limits = c(-5.20, 20.75),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.45, 10.75),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    tag = "d"
  ) +
  theme_minimal(base_family = font_family, base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 3)),
    plot.subtitle = element_blank(),
    plot.tag = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = "black",
      size = 12
    ),
    axis.title.x = element_text(size = 12, margin = margin(t = 3)),
    legend.position = "bottom",
    legend.title = element_text(size = 12, margin = margin(b = 2)),
    legend.text = element_text(size = 12),
    legend.margin = margin(-4, 0, 0, 0),
    legend.box.margin = margin(0),
    legend.box.spacing = unit(0, "pt"),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(1, 5, -2, 1)
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(72, "mm"),
    barheight = unit(3, "mm")
  ))

# ============================================================
# 6. Assemble and export
# ============================================================

top_row <- p_b + p_a +
  plot_layout(widths = c(0.90, 1.10))

bottom_row <- p_c + p_d +
  plot_layout(widths = c(1.05, 0.95))

combined_plot <- top_row / bottom_row +
  plot_layout(heights = c(1, 1.02)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  )

ggsave(
  filename = output_path,
  plot = combined_plot,
  width = 230,
  height = 167,
  units = "mm",
  device = svglite::svglite,
  bg = "transparent"
)

message("Saved: ", output_path)
message(
  "Panel d reconciliation (percentage points):\n",
  paste(
    reconciliation$region,
    sprintf("%.3f", reconciliation$ContributionPP),
    collapse = "\n"
  )
)
