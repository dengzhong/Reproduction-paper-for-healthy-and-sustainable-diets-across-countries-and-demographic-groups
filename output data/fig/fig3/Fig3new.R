library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)

# ============================================================
# 0. Paths
# ============================================================

script_arg <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

script_dir <- if (length(script_arg) == 1) {
  dirname(
    normalizePath(
      sub("^--file=", "", script_arg),
      winslash = "/",
      mustWork = TRUE
    )
  )
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

fig3a_path <- file.path(script_dir, "00fig3a.xlsx")
out_new_svg <- file.path(script_dir, "Fig3new.svg")
out_new_png <- file.path(script_dir, "Fig3new.png")

# Reuse the original calculation logic and panels.
source(file.path(script_dir, "Fig3.R"), chdir = TRUE)

# Typography used by the redesigned panels.
label_text_size_mm <- 10 / 2.845

# ============================================================
# 1. New panel a: country-level ranked change
#    (Optimized - Current) / Current * 100
# ============================================================

environment_map_labels <- c(
  "GHG" = "GHG emissions",
  "Land" = "Land use",
  "Freshwater" = "Water use",
  "Eutr." = "Eutrophication",
  "Acid." = "Acidification"
)

country_raw <- read_excel(fig3a_path)

required_columns <- c("ISO3", "Scenario", "Environment", "Total")
missing_columns <- setdiff(required_columns, names(country_raw))

if (length(missing_columns) > 0) {
  stop(
    "00fig3a.xlsx is missing required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

country_long <- country_raw %>%
  transmute(
    ISO3 = as.character(ISO3),
    Scenario = as.character(Scenario),
    Environment = as.character(Environment),
    Total = as.numeric(Total)
  )

required_scenarios <- c("Current", "Optimized")
missing_scenarios <- setdiff(
  required_scenarios,
  unique(country_long$Scenario)
)

if (length(missing_scenarios) > 0) {
  stop(
    "00fig3a.xlsx is missing required scenario(s): ",
    paste(missing_scenarios, collapse = ", ")
  )
}

duplicate_rows <- country_long %>%
  filter(
    Scenario %in% required_scenarios,
    Environment %in% environment_levels
  ) %>%
  count(ISO3, Environment, Scenario) %>%
  filter(n != 1)

if (nrow(duplicate_rows) > 0) {
  stop(
    "Each ISO3-Environment-Scenario combination must occur ",
    "exactly once in 00fig3a.xlsx."
  )
}

country_change <- country_long %>%
  filter(
    Scenario %in% required_scenarios,
    Environment %in% environment_levels
  ) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Total
  )

if (any(is.na(country_change$Current)) ||
    any(is.na(country_change$Optimized))) {
  stop("Current or Optimized is missing for at least one country.")
}

if (any(country_change$Current == 0)) {
  stop("Current equals zero; percentage change is undefined.")
}

country_change <- country_change %>%
  mutate(
    PctChange = (Optimized - Current) / Current * 100,
    Environment = factor(
      Environment,
      levels = environment_levels
    )
  )

impact_unit_labels <- c(
  "GHG" = "Gt CO2-eq",
  "Land" = "Mha",
  "Freshwater" = "km3",
  "Eutr." = "Mt PO4-eq",
  "Acid." = "Mt SO2-eq"
)

country_total_annotation <- country_long %>%
  filter(
    Scenario %in% required_scenarios,
    Environment %in% environment_levels
  ) %>%
  left_join(environment_info, by = "Environment") %>%
  group_by(Environment, Scenario, Divisor) %>%
  summarise(
    ImpactValue = sum(Total, na.rm = TRUE) * 365 / first(Divisor),
    .groups = "drop"
  ) %>%
  select(-Divisor) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = ImpactValue
  ) %>%
  mutate(
    Environment = factor(
      Environment,
      levels = environment_levels
    ),
    Unit = unname(impact_unit_labels[as.character(Environment)]),
    Label = paste0(
      "Current: ", sprintf("%.1f", Current), "\n",
      "Optimized: ", sprintf("%.1f", Optimized), "\n",
      Unit
    )
  )

change_limit <- ceiling(
  max(abs(country_change$PctChange), na.rm = TRUE) / 10
) * 10

country_rank_df <- country_change %>%
  group_by(Environment) %>%
  arrange(PctChange, ISO3, .by_group = TRUE) %>%
  mutate(
    CountryRank = row_number(),
    CountryCount = n(),
    PositiveLabel = if_else(
      PctChange > 0,
      ISO3,
      NA_character_
    )
  ) %>%
  ungroup()

print(
  country_change %>%
    group_by(Environment) %>%
    summarise(
      Countries = n(),
      MinPct = min(PctChange),
      MedianPct = median(PctChange),
      MaxPct = max(PctChange),
      .groups = "drop"
    )
)

p_country_rank <- ggplot(
  country_rank_df,
  aes(x = CountryRank, y = PctChange)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.28
  ) +
  geom_line(
    colour = "grey65",
    linewidth = 0.32
  ) +
  geom_point(
    aes(fill = PctChange),
    shape = 21,
    colour = "#4D4D4D",
    size = 1.25,
    alpha = 0.92,
    stroke = 0.25
  ) +
  ggrepel::geom_text_repel(
    data = country_rank_df %>%
      filter(
        !is.na(PositiveLabel)
      ),
    aes(label = PositiveLabel),
    family = font_family,
    fontface = "bold",
    size = 8 / 2.845,
    colour = "#263238",
    box.padding = 0.16,
    point.padding = 0.10,
    min.segment.length = 0,
    segment.colour = "grey55",
    segment.size = 0.20,
    max.overlaps = Inf,
    seed = 123,
    show.legend = FALSE
  ) +
  geom_text(
    data = country_total_annotation,
    aes(
      x = 5,
      y = -5,
      label = Label
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    family = font_family,
    fontface = "bold",
    size = 8 / 2.845,
    lineheight = 0.95,
    colour = "#263238"
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    ncol = 5,
    labeller = labeller(
      Environment = environment_map_labels
    )
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-change_limit, change_limit),
    oob = scales::squish,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(1, 82, 163),
    labels = c("1", "82", "163"),
    expand = expansion(mult = c(0.035, 0.035))
  ) +
  scale_y_continuous(
    limits = c(-100, 50),
    breaks = c(-100, -50, 0, 50),
    labels = format_pct_axis,
    oob = scales::squish,
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "a  Country-level change",
    x = "Country rank",
    y = "Change from current (%)"
  ) +
  theme_bw(
    base_size = main_text_size_pt,
    base_family = font_family
  ) +
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold",
      colour = "black",
      margin = margin(b = 2)
    ),
    strip.background = element_rect(
      fill = "#F3F3F3",
      colour = "#666666",
      linewidth = 0.30
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey90",
      linewidth = 0.28
    ),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.28
    ),
    panel.border = element_rect(
      colour = "#555555",
      fill = NA,
      linewidth = 0.30
    ),
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    axis.title = element_text(
      size = 12,
      colour = "black"
    ),
    legend.position = "none",
    panel.spacing.x = grid::unit(5, "mm"),
    panel.spacing.y = grid::unit(0, "mm"),
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0,
      margin = margin(b = 3)
    ),
    plot.margin = margin(2, 6, 3, 4)
  )

# ============================================================
# 2. Panel c: sex contribution
#    Same contribution logic as income and age groups
# ============================================================

change_sex_df <- fig3a %>%
  filter(region == "World") %>%
  group_by(Environment, Sex, Scenario, FoodGroup) %>%
  summarise(
    Value = sum(AnnualImpact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Value,
    values_fill = 0
  ) %>%
  mutate(
    Change = OD - CD,
    Sex = factor(Sex, levels = c("Male", "Female"))
  ) %>%
  group_by(Environment) %>%
  mutate(
    GlobalChange = sum(Change, na.rm = TRUE),
    PctGlobal = if_else(
      GlobalChange == 0,
      0,
      Change / GlobalChange * 100
    )
  ) %>%
  ungroup()

sex_label_df <- change_sex_df %>%
  group_by(Environment, Sex) %>%
  summarise(
    Contribution = sum(PctGlobal, na.rm = TRUE),
    Positive = sum(PctGlobal[PctGlobal > 0], na.rm = TRUE),
    Negative = sum(PctGlobal[PctGlobal < 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Label = sprintf("%.1f", Contribution),
    LabelY = if_else(Contribution >= 0, Positive, Negative),
    LabelVjust = if_else(Contribution >= 0, -0.25, 1.15)
  )

p_sex_contribution <- ggplot(
  change_sex_df,
  aes(x = Sex, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.56,
    colour = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = sex_label_df,
    aes(x = Sex, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = contribution_dot_size,
    stroke = 0.40,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = sex_label_df,
    aes(
      x = Sex,
      y = LabelY,
      label = Label,
      vjust = LabelVjust
    ),
    inherit.aes = FALSE,
    size = label_text_size_mm,
    family = font_family,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    labels = c("Male" = "M", "Female" = "F"),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = format_pct_axis,
    breaks = signed_breaks,
    expand = expansion(mult = c(0.10, 0.16))
  ) +
  labs(
    x = NULL,
    y = "Contribution (%)"
  ) +
  base_theme +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(size = x_text_size_pt),
    plot.margin = margin(2, 3, 3, 3),
    legend.position = "none"
  )

# ============================================================
# 3. Combine
#    a: country-ranked percentage change
#    b: income-group contribution
#    c: sex contribution
#    d: age-group contribution
# ============================================================

p_contribution_clean <- ggplot(
  change_food_df,
  aes(x = Income, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.62,
    colour = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = income_label_df,
    aes(x = Income, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.05,
    stroke = 0.40,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = income_label_df,
    aes(
      x = Income,
      y = LabelY,
      label = Label
    ),
    inherit.aes = FALSE,
    hjust = -0.16,
    size = label_text_size_mm,
    family = font_family,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    scales = "fixed"
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    limits = rev(income_levels),
    labels = income_labels,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80),
    labels = format_pct_axis,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_flip(
    ylim = c(-15, 85),
    clip = "off"
  ) +
  labs(
    title = "b  Income groups",
    x = NULL,
    y = "Contribution (%)"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(
      size = 12,
      colour = "black"
    ),
    axis.text.x = element_text(
      size = 12,
      colour = "black"
    ),
    axis.ticks.x = element_line(
      colour = "black",
      linewidth = 0.30
    ),
    axis.title.x = element_text(
      size = 12,
      colour = "black"
    ),
    panel.border = element_rect(
      colour = "#555555",
      fill = NA,
      linewidth = 0.30
    ),
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0,
      margin = margin(b = 2)
    ),
    panel.spacing.x = grid::unit(5, "mm"),
    plot.margin = margin(2, 6, 3, 4)
  )

p_sex_contribution_clean <- ggplot(
  change_sex_df,
  aes(x = Sex, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.62,
    colour = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = sex_label_df,
    aes(x = Sex, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.05,
    stroke = 0.40,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = sex_label_df,
    aes(
      x = Sex,
      y = LabelY,
      label = Label
    ),
    inherit.aes = FALSE,
    hjust = -0.16,
    size = label_text_size_mm,
    family = font_family,
    fontface = "bold"
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    scales = "fixed"
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    limits = c("Female", "Male"),
    labels = c("Male" = "M", "Female" = "F"),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80),
    labels = format_pct_axis,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_flip(
    ylim = c(-15, 85),
    clip = "off"
  ) +
  labs(
    title = "c  Sex",
    x = NULL,
    y = "Contribution (%)"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey90",
      linewidth = 0.28
    ),
    axis.text.y = element_text(
      size = 12,
      colour = "black"
    ),
    axis.text.x = element_text(
      size = 12,
      colour = "black"
    ),
    panel.border = element_rect(
      colour = "#555555",
      fill = NA,
      linewidth = 0.30
    ),
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0,
      margin = margin(b = 2)
    ),
    panel.spacing.x = grid::unit(5, "mm"),
    plot.margin = margin(2, 6, 3, 4)
  )

p_age_contribution_clean <- ggplot(
  change_age_df,
  aes(x = Age, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.78,
    colour = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = age_label_df,
    aes(x = Age, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = 1.60,
    stroke = 0.35,
    fill = "white",
    colour = "black"
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    scales = "fixed"
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels,
    drop = FALSE
  ) +
  scale_x_discrete(
    breaks = c(
      "0-4", "40-44", "80+"
    ),
    labels = c(
      "0-4", "40-44", "80+"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = format_pct_axis,
    breaks = signed_breaks,
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  coord_cartesian(
    ylim = c(-6, 14),
    clip = "off"
  ) +
  labs(
    title = "d  Age groups",
    x = NULL,
    y = "Contribution (%)"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(
      colour = "#555555",
      fill = NA,
      linewidth = 0.30
    ),
    axis.text.x = element_text(
      size = 12,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    ),
    panel.spacing.x = grid::unit(5, "mm"),
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0,
      margin = margin(b = 2)
    ),
    plot.margin = margin(2, 6, 3, 4)
  )

legend_fig_new <- extract_bottom_legend(
  p_abs +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = element_text(size = 12),
      legend.key.size = grid::unit(3.8, "mm")
    ) +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE)
    )
)

aligned_rows_new <- cowplot::align_plots(
  p_country_rank,
  p_contribution_clean,
  p_sex_contribution_clean,
  p_age_contribution_clean,
  align = "v",
  axis = "lr",
  greedy = FALSE
)

p_main_new <- cowplot::plot_grid(
  plotlist = aligned_rows_new,
  ncol = 1,
  rel_heights = c(1.30, 0.95, 0.63, 1.25)
)

p_fig3new <- cowplot::plot_grid(
  p_main_new,
  legend_fig_new,
  ncol = 1,
  rel_heights = c(1, 0.085)
)

print(p_fig3new)

ggsave(
  filename = out_new_svg,
  plot = p_fig3new,
  width = 230,
  height = 235,
  units = "mm",
  bg = "white"
)

ggsave(
  filename = out_new_png,
  plot = p_fig3new,
  width = 230,
  height = 235,
  units = "mm",
  dpi = 300,
  bg = "white"
)

message("Saved: ", out_new_svg)
message("Saved: ", out_new_png)
