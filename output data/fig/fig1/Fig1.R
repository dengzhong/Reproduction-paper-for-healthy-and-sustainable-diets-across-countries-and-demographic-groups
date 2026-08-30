# Figure 1: demographic variation in current and optimized diets
# Layout: two compact summary panels above one wide income-group comparison.

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "cowplot",
  "scales", "svglite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required R packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(scales)
})

# Paths and output settings --------------------------------------------------

get_script_dir <- function() {
  file_arg <- grep(
    "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE
  )

  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }

  normalizePath(getwd())
}

script_dir <- get_script_dir()
input_file <- file.path(script_dir, "fig1.xlsx")
svg_file <- file.path(script_dir, "Fig1new.svg")

figure_width_mm <- 230
figure_height_mm <- 300
base_font <- "Arial"
text_size_pt <- 12
geom_text_size <- text_size_pt / ggplot2::.pt

# Figure vocabulary ----------------------------------------------------------

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
  "65-69", "70-74", "75-79", "80+"
)

income_levels <- c(
  "High income", "Upper-middle income", "Lower middle income", "Low income"
)

income_labels <- c(
  "High income" = "High",
  "Upper-middle income" = "Upper-middle",
  "Lower middle income" = "Lower-middle",
  "Low income" = "Low"
)

food_colors <- c(
  "Seafood" = "#5996C0",
  "Dairy & eggs" = "#C19FBD",
  "Meat" = "#C05849",
  "Sugar & oil" = "#D08788",
  "Staple foods" = "#D29F4B",
  "Legumes & nuts" = "#228A76",
  "Fruits" = "#C5DCB1",
  "Vegetable" = "#90B9A4"
)

food_levels <- names(food_colors)
food_labels <- c(
  "Seafood" = "Seafood",
  "Dairy & eggs" = "Dairy & egg",
  "Meat" = "Meat",
  "Sugar & oil" = "Sugar & oil",
  "Staple foods" = "Staple food",
  "Legumes & nuts" = "Legu. & nut",
  "Fruits" = "Fruit",
  "Vegetable" = "Vegetable"
)

bar_levels <- c(
  "FML__Current diets", "FML__Optimized diets",
  "MLE__Current diets", "MLE__Optimized diets"
)

bar_labels <- c(
  "FML__Current diets" = "Current",
  "FML__Optimized diets" = "Optimized",
  "MLE__Current diets" = "Current",
  "MLE__Optimized diets" = "Optimized"
)

sex_labels <- c("FML" = "Female", "MLE" = "Male")
positive_color <- "#2F806A"
negative_color <- "#B85B4A"

theme_figure <- function() {
  theme_classic(base_size = text_size_pt, base_family = base_font) +
    theme(
      axis.title = element_text(size = text_size_pt, colour = "black"),
      axis.text = element_text(size = text_size_pt, colour = "black"),
      axis.line = element_line(linewidth = 0.38, colour = "black"),
      legend.text = element_text(size = text_size_pt),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# Read and validate data -----------------------------------------------------

if (!file.exists(input_file)) {
  stop("Missing input workbook: ", input_file, call. = FALSE)
}

data_raw <- read_excel(input_file, sheet = "Sheet1")
required_columns <- c("Scenario", "Age", "Sex", "FoodGroup", "region", "Intake")
missing_columns <- setdiff(required_columns, names(data_raw))

if (length(missing_columns) > 0L) {
  stop(
    "fig1.xlsx is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

dat <- data_raw |>
  transmute(
    Scenario = as.character(Scenario),
    Age = as.character(Age),
    Sex = as.character(Sex),
    FoodGroup = as.character(FoodGroup),
    region = as.character(region),
    Intake = as.numeric(Intake)
  )

if (any(!is.finite(dat$Intake))) {
  stop("Intake contains missing or non-finite values.", call. = FALSE)
}

duplicate_rows <- dat |>
  count(Scenario, Age, Sex, FoodGroup, region, name = "n") |>
  filter(n > 1L)

if (nrow(duplicate_rows) > 0L) {
  stop(
    "Duplicate Scenario-Age-Sex-FoodGroup-region records were found.",
    call. = FALSE
  )
}

required_scenarios <- c("Current diets", "Optimized diets")
if (!all(required_scenarios %in% dat$Scenario)) {
  stop("Both Current diets and Optimized diets are required.", call. = FALSE)
}

changes <- dat |>
  filter(Scenario %in% required_scenarios) |>
  select(Scenario, Age, Sex, FoodGroup, region, Intake) |>
  pivot_wider(names_from = Scenario, values_from = Intake)

change_rows_used <- changes |>
  filter(
    FoodGroup %in% food_levels,
    Sex %in% names(sex_labels),
    (region == "World" & Age %in% c("All", age_levels)) |
      (region %in% income_levels & Age == "All")
  )

if (any(change_rows_used[["Current diets"]] <= 0, na.rm = TRUE)) {
  stop(
    "Figure 1 percentage changes require positive Current diets values.",
    call. = FALSE
  )
}

changes <- changes |>
  mutate(
    pct_change = if_else(
      `Current diets` > 0,
      (`Optimized diets` - `Current diets`) / `Current diets` * 100,
      NA_real_
    )
  )

# Panel a: global energy and food-group composition --------------------------

panel_a_data <- dat |>
  filter(
    region == "World",
    Age == "All",
    Sex %in% names(sex_labels),
    Scenario %in% required_scenarios,
    FoodGroup %in% food_levels
  ) |>
  mutate(
    Bar = factor(paste(Sex, Scenario, sep = "__"), levels = bar_levels),
    FoodGroup = factor(FoodGroup, levels = food_levels)
  )

panel_a_totals <- dat |>
  filter(
    region == "World",
    Age == "All",
    FoodGroup == "Total",
    Sex %in% names(sex_labels),
    Scenario %in% required_scenarios
  ) |>
  mutate(Bar = factor(paste(Sex, Scenario, sep = "__"), levels = bar_levels))

if (nrow(panel_a_data) != length(bar_levels) * length(food_levels)) {
  stop("Panel a requires four bars and all eight food groups.", call. = FALSE)
}
if (nrow(panel_a_totals) != length(bar_levels)) {
  stop("Panel a requires one Total record for each bar.", call. = FALSE)
}

max_total <- max(panel_a_totals$Intake)
y_breaks <- pretty(c(0, max_total), n = 6)
y_breaks <- y_breaks[y_breaks >= 0 & y_breaks <= max(y_breaks[y_breaks <= max_total])]
y_tick_top <- max(y_breaks)
if (y_tick_top < max_total) {
  y_tick_top <- max(pretty(c(0, max_total), n = 6))
  y_breaks <- pretty(c(0, y_tick_top), n = 6)
}
y_limit <- y_tick_top * 1.22
bracket_y <- y_tick_top * 1.135
sex_label_y <- y_tick_top * 1.18

panel_a <- ggplot(panel_a_data, aes(Bar, Intake, fill = FoodGroup)) +
  geom_col(
    width = 0.72,
    colour = "white",
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = panel_a_totals,
    aes(Bar, Intake + y_tick_top * 0.035, label = round(Intake)),
    inherit.aes = FALSE,
    family = base_font,
    fontface = "bold",
    size = geom_text_size,
    vjust = 0
  ) +
  annotate(
    "segment",
    x = c(0.65, 2.65), xend = c(2.35, 4.35),
    y = bracket_y, yend = bracket_y,
    linewidth = 0.32, colour = "#666666"
  ) +
  annotate(
    "text",
    x = c(1.5, 3.5), y = sex_label_y,
    label = c("Female", "Male"),
    family = base_font, fontface = "bold", size = geom_text_size
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels[food_levels],
    name = NULL
  ) +
  scale_x_discrete(labels = bar_labels) +
  scale_y_continuous(
    limits = c(0, y_limit),
    breaks = y_breaks,
    expand = expansion(mult = c(0, 0)),
    labels = label_number(big.mark = "", accuracy = 1)
  ) +
  labs(x = NULL, y = "Energy intake (kcal/cap/d)") +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_figure() +
  theme(
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.text.x = element_text(lineheight = 0.9),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#D9D9D9", linewidth = 0.27),
    legend.position = "bottom",
    legend.key.height = grid::unit(1.6, "mm"),
    legend.key.width = grid::unit(2.4, "mm"),
    legend.spacing.x = grid::unit(2.5, "mm"),
    legend.spacing.y = grid::unit(0, "mm"),
    legend.box.spacing = grid::unit(5.5, "mm"),
    plot.margin = margin(4, 1, 0, 1)
  )

# Panel b: deviations by income group ---------------------------------------

group_gap <- 1.35
group_stride <- length(food_levels) + group_gap

panel_b_data <- changes |>
  filter(
    Age == "All",
    region %in% income_levels,
    Sex %in% names(sex_labels),
    FoodGroup %in% food_levels
  ) |>
  mutate(
    region = factor(region, levels = income_levels),
    FoodGroup = factor(FoodGroup, levels = food_levels),
    Sex = factor(Sex, levels = names(sex_labels), labels = sex_labels),
    region_id = as.integer(region),
    food_id = as.integer(FoodGroup),
    y_num = (region_id - 1) * group_stride + food_id
  ) |>
  group_by(region, FoodGroup) |>
  mutate(
    pct_label = sprintf("%.0f%%", pct_change),
    label_hjust = case_when(
      pct_change < max(pct_change) ~ 1.25,
      pct_change > min(pct_change) ~ -0.25,
      Sex == "Female" ~ 1.25,
      TRUE ~ -0.25
    )
  ) |>
  ungroup()

expected_b_rows <- length(income_levels) * length(food_levels) * 2L
if (nrow(panel_b_data) != expected_b_rows) {
  stop(
    "Panel b requires Female and Male values for eight foods in four income groups.",
    call. = FALSE
  )
}

panel_b_lines <- panel_b_data |>
  select(region, FoodGroup, y_num, Sex, pct_change) |>
  pivot_wider(names_from = Sex, values_from = pct_change) |>
  mutate(
    xmin = pmin(Female, Male),
    xmax = pmax(Female, Male),
    direction = if_else((Female + Male) / 2 >= 0, "Increase", "Reduction")
  )

b_y_breaks <- panel_b_data |>
  distinct(region, FoodGroup, y_num) |>
  arrange(y_num)

region_centres <- panel_b_data |>
  group_by(region) |>
  summarise(y = mean(range(y_num)), .groups = "drop")

b_min <- min(panel_b_data$pct_change)
b_max <- max(panel_b_data$pct_change)
b_x_min <- min(-105, floor(b_min / 50) * 50 - 5)
b_x_max <- max(365, ceiling(b_max / 50) * 50 + 15)
b_x_breaks <- seq(
  ceiling(b_x_min / 100) * 100,
  floor(b_x_max / 100) * 100,
  by = 100
)
income_label_x <- b_x_min - 0.23 * (b_x_max - b_x_min)

separator_y <- seq_len(length(income_levels) - 1L) * group_stride - group_gap / 2
shaded_groups <- c(2L, 4L)
shade_rects <- tibble(
  xmin = b_x_min,
  xmax = b_x_max,
  ymin = (shaded_groups - 1) * group_stride + 0.5 - group_gap / 2,
  ymax = (shaded_groups - 1) * group_stride + length(food_levels) +
    0.5 + group_gap / 2
)

panel_b_plot <- ggplot() +
  geom_rect(
    data = shade_rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#F2F2F1", colour = NA
  ) +
  geom_hline(
    yintercept = separator_y,
    linewidth = 0.28,
    colour = "#BEBEBE"
  ) +
  geom_segment(
    data = panel_b_lines,
    aes(x = xmin, xend = xmax, y = y_num, yend = y_num, colour = direction),
    linewidth = 0.75,
    lineend = "round"
  ) +
  geom_point(
    data = panel_b_data,
    aes(x = pct_change, y = y_num, shape = Sex, fill = Sex),
    size = 2,
    stroke = 0.45,
    colour = "#252525"
  ) +
  geom_text(
    data = filter(panel_b_data, Sex == "Female"),
    aes(
      x = pct_change, y = y_num, label = pct_label,
      hjust = label_hjust
    ),
    family = base_font,
    fontface = "bold",
    size = 10 / ggplot2::.pt,
    colour = "#1F77B4",
    vjust = 0.45
  ) +
  geom_text(
    data = filter(panel_b_data, Sex == "Male"),
    aes(
      x = pct_change, y = y_num, label = pct_label,
      hjust = label_hjust
    ),
    family = base_font,
    fontface = "bold",
    size = 10 / ggplot2::.pt,
    colour = "#D62728",
    vjust = 0.45
  ) +
  geom_vline(xintercept = 0, linewidth = 0.42, colour = "#444444") +
  annotate(
    "text",
    x = income_label_x,
    y = region_centres$y,
    label = income_labels[as.character(region_centres$region)],
    angle = 90,
    family = base_font,
    fontface = "bold",
    size = geom_text_size
  ) +
  scale_colour_manual(
    values = c("Increase" = positive_color, "Reduction" = negative_color)
  ) +
  scale_shape_manual(
    values = c("Female" = 21, "Male" = 22)
  ) +
  scale_fill_manual(
    values = c("Female" = "white", "Male" = "#484848")
  ) +
  scale_x_continuous(
    breaks = b_x_breaks,
    expand = expansion(mult = c(0, 0)),
    labels = label_number(big.mark = "", accuracy = 1)
  ) +
  scale_y_reverse(
    breaks = b_y_breaks$y_num,
    labels = food_labels[as.character(b_y_breaks$FoodGroup)],
    expand = expansion(add = c(1.15, 0.80))
  ) +
  coord_cartesian(xlim = c(b_x_min, b_x_max), clip = "off") +
  labs(x = "Deviations from current diets (%)", y = NULL) +
  theme_figure() +
  theme(
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text.y = element_text(margin = margin(r = 14)),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#DADADA", linewidth = 0.28),
    legend.position = "none",
    plot.margin = margin(0, 2, 1, 49)
  )

# Panel c: age-specific bubble matrices -------------------------------------

panel_c_data <- changes |>
  filter(
    region == "World",
    Age %in% age_levels,
    Sex %in% names(sex_labels),
    FoodGroup %in% food_levels
  ) |>
  mutate(
    Age = factor(Age, levels = rev(age_levels)),
    FoodGroup = factor(FoodGroup, levels = food_levels),
    Sex = factor(Sex, levels = names(sex_labels), labels = sex_labels),
    bubble_size = 0.27 * sqrt(18 + pmin(abs(pct_change), 350) / 350 * 185)
  )

expected_c_rows <- length(age_levels) * length(food_levels) * 2L
if (nrow(panel_c_data) != expected_c_rows) {
  stop(
    "Panel c requires both sexes, all 17 ages, and all eight food groups.",
    call. = FALSE
  )
}

panel_c_base <- ggplot(panel_c_data, aes(FoodGroup, Age)) +
  geom_point(
    aes(size = bubble_size, fill = pct_change),
    shape = 21,
    colour = "#818181",
    stroke = 0.28
  ) +
  facet_wrap(~Sex, nrow = 1) +
  scale_x_discrete(labels = food_labels, expand = expansion(add = 0.48)) +
  scale_y_discrete(expand = expansion(add = 0.40)) +
  scale_size_identity() +
  scale_fill_gradientn(
    colours = c("#A9463E", "#E3B7AF", "#FAFAF8", "#CFE4DC", "#2F806A"),
    values = scales::rescale(c(-100, -50, 0, 175, 350), from = c(-100, 350)),
    limits = c(-100, 350),
    oob = squish,
    breaks = c(-100, 0, 100, 200, 300),
    name = "Deviation from current diets (%)"
  ) +
  labs(x = NULL, y = "Age groups") +
  theme_minimal(base_size = text_size_pt, base_family = base_font) +
  theme(
    panel.grid.major = element_line(colour = "#E2E2E2", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = text_size_pt,
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      colour = "black"
    ),
    axis.text.y = element_text(size = text_size_pt, colour = "black"),
    axis.title.y = element_text(size = text_size_pt, colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(
      family = base_font,
      face = "bold",
      size = text_size_pt,
      hjust = 0.5
    ),
    panel.spacing.x = grid::unit(5, "mm"),
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "white", colour = NA)
  )

panel_c_matrix <- panel_c_base + theme(legend.position = "none")

svglite::svglite(
  svg_file,
  width = figure_width_mm / 25.4,
  height = figure_height_mm / 25.4,
  bg = "white"
)

panel_b_core <- panel_b_plot +
  theme(
    legend.position = "none",
    plot.margin = margin(2, 2, 1, 49)
  )

colour_legend <- cowplot::get_legend(
  panel_c_base +
    guides(
      size = "none",
      fill = guide_colourbar(
        title.position = "right",
        barheight = grid::unit(47, "mm"),
        barwidth = grid::unit(3.3, "mm"),
        ticks = TRUE,
        frame.colour = "black",
        frame.linewidth = 0.3
      )
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = text_size_pt,
        angle = 90,
        vjust = 0.5
      ),
      legend.text = element_text(size = text_size_pt),
      legend.spacing.x = grid::unit(1, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

sex_legend <- tibble(
  x = c(2, 27),
  label_x = c(6, 31),
  label = c("Female", "Male")
)

direction_legend <- tibble(
  x = c(49, 79),
  xend = c(55, 85),
  label_x = c(58, 88),
  label = c("Increase", "Reduction"),
  colour = c(positive_color, negative_color)
)

magnitude_values <- c(50, 150, 300)
magnitude_legend <- tibble(
  x = c(112, 130, 149),
  label_x = c(116, 135, 155),
  label = paste0(magnitude_values, "%"),
  size = 0.85 * 0.27 * sqrt(18 + magnitude_values / 350 * 185)
)

shared_legend <- ggplot() +
  geom_point(
    data = sex_legend,
    aes(x, 0.5, shape = label, fill = label),
    size = 2.4,
    stroke = 0.45,
    colour = "#252525"
  ) +
  geom_text(
    data = sex_legend,
    aes(label_x, 0.5, label = label),
    hjust = 0,
    family = base_font,
    size = geom_text_size
  ) +
  geom_segment(
    data = direction_legend,
    aes(x = x, xend = xend, y = 0.5, yend = 0.5, colour = colour),
    linewidth = 1.0,
    lineend = "round"
  ) +
  geom_text(
    data = direction_legend,
    aes(label_x, 0.5, label = label),
    hjust = 0,
    family = base_font,
    size = geom_text_size
  ) +
  geom_point(
    data = magnitude_legend,
    aes(x, 0.5, size = size),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.28
  ) +
  geom_text(
    data = magnitude_legend,
    aes(label_x, 0.5, label = label),
    hjust = 0,
    family = base_font,
    size = geom_text_size
  ) +
  scale_shape_manual(values = c("Female" = 21, "Male" = 22)) +
  scale_fill_manual(values = c("Female" = "white", "Male" = "#484848")) +
  scale_colour_identity() +
  scale_size_identity() +
  guides(shape = "none", fill = "none", colour = "none", size = "none") +
  coord_cartesian(xlim = c(0, 169), ylim = c(0, 1), clip = "off") +
  theme_void(base_size = text_size_pt, base_family = base_font) +
  theme(plot.margin = margin(0, 0, 0, 0))

# Assemble panels ------------------------------------------------------------

panel_b <- ggdraw() +
  draw_plot(panel_b_core, x = 0, y = 0, width = 1, height = 0.91) +
  draw_plot(shared_legend, x = 0.225, y = 0.920, width = 0.755, height = 0.070)

panel_c <- ggdraw() +
  draw_plot(panel_c_matrix, x = 0.05, y = 0, width = 0.82, height = 0.98) +
  draw_grob(colour_legend, x = 0.934, y = 0.18, width = 0.066, height = 0.78)

full_figure <- ggdraw() +
  draw_plot(panel_a, x = 0.035, y = 0.604, width = 0.388, height = 0.369) +
  draw_plot(panel_c, x = 0.435, y = 0.604, width = 0.535, height = 0.378) +
  draw_plot(panel_b, x = 0.020, y = 0.0045, width = 0.950, height = 0.5905) +
  draw_label(
    "a", x = 0.043, y = 0.978, hjust = 0, vjust = 1,
    fontface = "bold", fontfamily = base_font, size = text_size_pt
  ) +
  draw_label(
    "c", x = 0.449, y = 0.978, hjust = 0, vjust = 1,
    fontface = "bold", fontfamily = base_font, size = text_size_pt
  ) +
  draw_label(
    "b", x = 0.043, y = 0.582, hjust = 0, vjust = 1,
    fontface = "bold", fontfamily = base_font, size = text_size_pt
  ) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

print(full_figure)
invisible(grDevices::dev.off())
message("Saved: ", normalizePath(svg_file))
