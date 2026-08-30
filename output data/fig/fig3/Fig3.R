library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)

# ============================================================
# 0. Global settings
# ============================================================

font_family <- "sans"

main_text_size_pt <- 12
x_text_size_pt <- 12
facet_text_size_pt <- 12
axis_title_size_pt <- 12
label_text_size_mm <- 3.4
border_lwd <- 0.35

contribution_dot_col <- "#E41A1C"
contribution_dot_size <- 1.8
contribution_dot_stroke <- 0.25

region_levels <- c(
  "World",
  "High income",
  "Upper-middle income",
  "Lower middle income",
  "Low income"
)

income_levels <- c(
  "Low income",
  "Lower middle income",
  "Upper-middle income",
  "High income"
)

income_labels <- c(
  "Low income" = "L",
  "Lower middle income" = "LM",
  "Upper-middle income" = "UM",
  "High income" = "H"
)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

environment_levels <- c(
  "GHG",
  "Land",
  "Freshwater",
  "Eutr.",
  "Acid."
)

environment_labels <- c(
  "GHG" = "GHG emissions",
  "Land" = "Land use",
  "Freshwater" = "Water",
  "Eutr." = "Eutrop.",
  "Acid." = "Acid."
)

food_levels <- c(
  "Seafood",
  "Dairy & eggs",
  "Meat",
  "Sugar & oil",
  "Staple foods",
  "Legumes & nuts",
  "Fruits",
  "Vegetable"
)

food_labels <- c(
  "Seafood" = "Fish",
  "Dairy & eggs" = "Dairy & eggs",
  "Meat" = "Meat",
  "Sugar & oil" = "Sugar & oils",
  "Staple foods" = "Staple food",
  "Legumes & nuts" = "Legumes & nuts",
  "Fruits" = "Fruits",
  "Vegetable" = "Vegetables"
)

food_colors <- c(
  "Seafood" = "#5A9DCA",
  "Dairy & eggs" = "#C3A0C1",
  "Meat" = "#C85A4A",
  "Sugar & oil" = "#D08788",
  "Staple foods" = "#D8A24A",
  "Legumes & nuts" = "#238E79",
  "Fruits" = "#C7DFAF",
  "Vegetable" = "#91BBA7"
)

environment_info <- tibble(
  Environment = environment_levels,
  Divisor = c(1e15, 1e13, 1e12, 1e12, 1e12)
)

format_axis_no_sep <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = "")
}

format_total_no_sep <- function(x) {
  formatC(x, format = "f", digits = 1, big.mark = "")
}

format_pct_axis <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = "")
}

consistent_breaks_abs <- function(x) {
  upper <- max(x, na.rm = TRUE)

  if (!is.finite(upper) || upper <= 0) {
    return(0)
  }

  raw_step <- upper / 4
  magnitude <- 10^floor(log10(raw_step))
  candidates <- c(1, 2, 4, 5, 10) * magnitude
  step <- min(candidates[candidates >= raw_step])
  upper_break <- floor(upper / step) * step

  seq(0, upper_break, by = step)
}

signed_breaks <- function(x) {
  value_range <- range(x, na.rm = TRUE)

  if (!all(is.finite(value_range))) {
    return(0)
  }

  max_abs <- max(abs(value_range), na.rm = TRUE)

  if (!is.finite(max_abs) || max_abs <= 0) {
    return(0)
  }

  raw_step <- max_abs / 4
  magnitude <- 10^floor(log10(raw_step))
  candidates <- c(1, 2, 4, 5, 10) * magnitude
  step <- min(candidates[candidates >= raw_step])

  seq(
    floor(value_range[1] / step) * step,
    ceiling(value_range[2] / step) * step,
    by = step
  )
}

extract_bottom_legend <- function(plot) {
  plot_grob <- ggplotGrob(plot)
  guide_boxes <- which(grepl("^guide-box", plot_grob$layout$name))

  for (guide_box in guide_boxes) {
    if (!inherits(plot_grob$grobs[[guide_box]], "zeroGrob")) {
      return(plot_grob$grobs[[guide_box]])
    }
  }

  stop("没有找到可用的图例。")
}

# ============================================================
# 1. Read and clean data
# ============================================================

data_dir <- "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/FoodBalance2013-/optimized/optmized/output/fig/fig3"
fig3_path <- file.path(data_dir, "00fig3.xlsx")
out_svg <- file.path(data_dir, "00Fig3.svg")

fig3a <- read_excel(fig3_path) %>%
  filter(
    Age != "All",
    Sex != "All",
    FoodGroup != "Total",
    region %in% region_levels
  ) %>%
  mutate(
    Scenario = recode(
      Scenario,
      "Current diets" = "CD",
      "Optimized diets" = "OD"
    ),
    Sex = recode(
      Sex,
      "MLE" = "Male",
      "FML" = "Female"
    )
  ) %>%
  left_join(environment_info, by = "Environment") %>%
  mutate(
    AnnualImpact = Intake * 365 / Divisor,
    Environment = factor(Environment, levels = environment_levels),
    FoodGroup = factor(FoodGroup, levels = food_levels),
    Age = factor(Age, levels = age_levels),
    Scenario = factor(Scenario, levels = c("CD", "OD"))
  )

# ============================================================
# 2. Panel a: CD and OD, each with World/Male/Female
# ============================================================

world_total <- fig3a %>%
  filter(region == "World") %>%
  group_by(Scenario, Environment, FoodGroup) %>%
  summarise(
    Value = sum(AnnualImpact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(SexGroup = "World")

sex_total <- fig3a %>%
  filter(region == "World") %>%
  group_by(Scenario, Environment, FoodGroup, Sex) %>%
  summarise(
    Value = sum(AnnualImpact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(SexGroup = Sex)

plot_abs_df <- bind_rows(world_total, sex_total) %>%
  mutate(
    SexGroup = factor(
      SexGroup,
      levels = c("World", "Male", "Female")
    ),
    X = case_when(
      Scenario == "CD" & SexGroup == "World"  ~ 1,
      Scenario == "CD" & SexGroup == "Male"   ~ 2,
      Scenario == "CD" & SexGroup == "Female" ~ 3,
      Scenario == "OD" & SexGroup == "World"  ~ 5,
      Scenario == "OD" & SexGroup == "Male"   ~ 6,
      Scenario == "OD" & SexGroup == "Female" ~ 7
    )
  )

total_abs_df <- plot_abs_df %>%
  group_by(Environment, Scenario, SexGroup, X) %>%
  summarise(
    Total = sum(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Label = format_total_no_sep(Total)
  )

scenario_label_df <- crossing(
  Environment = factor(
    environment_levels,
    levels = environment_levels
  ),
  tibble(
    X = c(2, 6),
    Label = c("Current", "Optimized")
  )
)

# World must equal Male + Female.
check_sex_total <- total_abs_df %>%
  select(Environment, Scenario, SexGroup, Total) %>%
  pivot_wider(names_from = SexGroup, values_from = Total) %>%
  mutate(Difference = World - Male - Female)

print(check_sex_total)

# ============================================================
# 3. Panel b: income-group contribution (unchanged logic)
# ============================================================

change_food_df <- fig3a %>%
  filter(region %in% income_levels) %>%
  group_by(Environment, region, Scenario, FoodGroup) %>%
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
    Income = factor(region, levels = income_levels)
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

income_label_df <- change_food_df %>%
  group_by(Environment, Income) %>%
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

# ============================================================
# 4. Panel c: age-group contribution (unchanged logic)
# ============================================================

change_age_df <- fig3a %>%
  filter(region == "World") %>%
  group_by(Environment, Age, Scenario, FoodGroup) %>%
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
    Change = OD - CD
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

age_label_df <- change_age_df %>%
  group_by(Environment, Age) %>%
  summarise(
    Contribution = sum(PctGlobal, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 5. Shared theme
# ============================================================

base_theme <- theme_bw(
  base_size = main_text_size_pt,
  base_family = font_family
) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey88",
      linewidth = 0.30
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = border_lwd
    ),
    panel.spacing.x = unit(4.2, "mm"),
    panel.spacing.y = unit(0, "mm"),
    axis.text = element_text(
      color = "black",
      size = main_text_size_pt
    ),
    axis.title = element_text(
      color = "black",
      size = axis_title_size_pt
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.30
    ),
    strip.background = element_rect(
      fill = "grey95",
      color = "black",
      linewidth = border_lwd
    ),
    strip.text = element_text(
      size = facet_text_size_pt,
      face = "bold",
      color = "black",
      lineheight = 1.0
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size = unit(3.8, "mm"),
    plot.margin = margin(1, 3, 1, 3)
  )

# ============================================================
# 6. Panel a
# ============================================================

p_abs <- ggplot(
  plot_abs_df,
  aes(x = X, y = Value, fill = FoodGroup)
) +
  geom_col(
    width = 0.68,
    color = NA,
    linewidth = 0
  ) +
  geom_vline(
    xintercept = 4,
    color = "grey78",
    linewidth = 0.35
  ) +
  geom_text(
    data = total_abs_df,
    aes(x = X, y = Total, label = Label),
    inherit.aes = FALSE,
    vjust = -0.28,
    size = label_text_size_mm,
    family = font_family,
    fontface = "bold"
  ) +
  geom_text(
    data = scenario_label_df,
    aes(x = X, y = -Inf, label = Label),
    inherit.aes = FALSE,
    vjust = 2.25,
    size = 12 / 2.845,
    family = font_family
  ) +
  facet_wrap(
    ~ Environment,
    nrow = 1,
    scales = "free_y",
    labeller = labeller(Environment = environment_labels)
  ) +
  scale_fill_manual(
    values = food_colors,
    breaks = food_levels,
    labels = food_labels,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0.45, 7.55),
    breaks = c(1, 2, 3, 5, 6, 7),
    labels = rep(c("W", "M", "F"), 2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = format_axis_no_sep,
    breaks = consistent_breaks_abs,
    expand = expansion(mult = c(0, 0.14))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Impact value"
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  base_theme +
  theme(
    axis.text.x = element_text(
      size = 12,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.ticks.x = element_blank(),
    plot.margin = margin(3, 3, 20, 3)
  )

# ============================================================
# 7. Panel b
# ============================================================

p_contribution <- ggplot(
  change_food_df,
  aes(x = Income, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.58,
    color = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = income_label_df,
    aes(x = Income, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = contribution_dot_size,
    stroke = contribution_dot_stroke,
    fill = contribution_dot_col,
    color = "black"
  ) +
  geom_text(
    data = income_label_df,
    aes(
      x = Income,
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
    labels = income_labels,
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
    plot.margin = margin(2, 3, 3, 3)
  )

# ============================================================
# 8. Panel c
# ============================================================

p_age_contribution <- ggplot(
  change_age_df,
  aes(x = Age, y = PctGlobal, fill = FoodGroup)
) +
  geom_col(
    width = 0.78,
    color = NA,
    linewidth = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.28
  ) +
  geom_point(
    data = age_label_df,
    aes(x = Age, y = Contribution),
    inherit.aes = FALSE,
    shape = 21,
    size = contribution_dot_size * 0.78,
    stroke = contribution_dot_stroke,
    fill = contribution_dot_col,
    color = "black"
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
    breaks = age_levels,
    labels = age_levels,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = format_pct_axis,
    breaks = signed_breaks,
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  labs(
    x = NULL,
    y = "Contribution (%)"
  ) +
  base_theme +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(
      size = 6.5,
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    plot.margin = margin(2, 3, 3, 3)
  )

# ============================================================
# 9. Combine and save
# ============================================================

legend_fig <- extract_bottom_legend(
  p_abs +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.justification = "center"
    ) +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE)
    )
)

p_abs_clean <- p_abs + theme(legend.position = "none")
p_contribution_clean <- p_contribution + theme(legend.position = "none")
p_age_contribution_clean <- p_age_contribution + theme(legend.position = "none")

aligned_plots <- cowplot::align_plots(
  p_abs_clean,
  p_contribution_clean,
  p_age_contribution_clean,
  align = "v",
  axis = "lr"
)

p_main <- cowplot::plot_grid(
  aligned_plots[[1]],
  aligned_plots[[2]],
  aligned_plots[[3]],
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1.18, 0.95, 1.25),
  labels = c("a", "b", "c"),
  label_size = 12,
  label_fontface = "bold",
  label_x = 0.002,
  label_y = 0.995,
  hjust = 0,
  vjust = 1
)

p_fig3 <- cowplot::plot_grid(
  p_main,
  legend_fig,
  ncol = 1,
  rel_heights = c(1, 0.13)
)

print(p_fig3)

ggsave(
  filename = out_svg,
  plot = p_fig3,
  width = 250,
  height = 180,
  units = "mm",
  bg = "white"
)
