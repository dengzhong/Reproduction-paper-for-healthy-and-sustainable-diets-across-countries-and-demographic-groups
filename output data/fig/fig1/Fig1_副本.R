# Reproduce Figure 1 from 00fig1.xlsx
# Output: Figure1_restructured_v3_R/Figure1_restructured_v3_R.svg

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "cowplot", "scales", "svglite"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Please install the required R packages first: ",
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

# cowplot otherwise uses a temporary PDF device for text metrics; its PNG null
# device resolves the installed Arial font correctly through the macOS device.
cowplot::set_null_device("png")

input_file <- "fig1.xlsx"
output_dir <- "~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig1"

base_font <- "Arial"
text_size_pt <- 12
figure_width_mm <- 230
figure_height_mm <- 300
# geom_text()/annotate("text") use millimetres, while theme text and
# cowplot::draw_label() use points.
geom_text_size <- text_size_pt / ggplot2::.pt

dat <- read_excel(input_file, sheet = "Sheet1") |>
  mutate(Intake = as.numeric(Intake))

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
  "65-69", "70-74", "75-79", "80+"
)

food_labels <- c(
  "Legumes & nuts" = "Legu. & nut",
  "Fruits" = "Fruit",
  "Vegetable" = "Vegetable",
  "Dairy & eggs" = "Dairy & egg",
  "Seafood" = "Seafood",
  "Staple foods" = "Staple food",
  "Sugar & oil" = "Sugar & oil",
  "Meat" = "Meat"
)

food_labels_c <- c(
  "Legumes & nuts" = "Legu. & nut",
  "Fruits" = "Fruit",
  "Vegetable" = "Vegetable",
  "Dairy & eggs" = "Dairy & egg",
  "Seafood" = "Seafood",
  "Staple foods" = "Staple food",
  "Sugar & oil" = "Sugar & oil",
  "Meat" = "Meat"
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

food_cols <- c(
  "Seafood" = "#5996c0",
  "Dairy & eggs" = "#c19fbd",
  "Meat" = "#c05849",
  "Sugar & oil" = "#d08788",
  "Staple foods" = "#d29f4b",
  "Legumes & nuts" = "#228a76",
  "Fruits" = "#c5dcb1",
  "Vegetable" = "#90b9a4"
)

# Use the named colour vector as the single source of truth for panel-a food
# order, stacking order, and legend order.
food_levels <- names(food_cols)

positive_color <- "#2F806A"
negative_color <- "#B85B4A"

changes <- dat |>
  filter(Scenario %in% c("Current diets", "Optimized diets")) |>
  select(Scenario, Age, Sex, FoodGroup, region, Intake) |>
  pivot_wider(names_from = Scenario, values_from = Intake) |>
  mutate(
    pct_change = (`Optimized diets` - `Current diets`) / `Current diets` * 100
  )

# -----------------------------------------------------------------------------
# Panel a: global dietary structure
# -----------------------------------------------------------------------------
panel_a_data <- dat |>
  filter(
    region == "World", Age == "All", Sex %in% c("FML", "MLE"),
    Scenario %in% c("Current diets", "Optimized diets"),
    FoodGroup %in% food_levels
  ) |>
  mutate(
    Bar = factor(
      paste(Sex, Scenario, sep = "__"),
      levels = c(
        "FML__Current diets", "FML__Optimized diets",
        "MLE__Current diets", "MLE__Optimized diets"
      )
    ),
    FoodGroup = factor(FoodGroup, levels = food_levels)
  )

panel_a_totals <- dat |>
  filter(
    region == "World", Age == "All", FoodGroup == "Total",
    Sex %in% c("FML", "MLE"),
    Scenario %in% c("Current diets", "Optimized diets")
  ) |>
  mutate(
    Bar = factor(
      paste(Sex, Scenario, sep = "__"),
      levels = c(
        "FML__Current diets", "FML__Optimized diets",
        "MLE__Current diets", "MLE__Optimized diets"
      )
    )
  )

panel_a <- ggplot(panel_a_data, aes(Bar, Intake, fill = FoodGroup)) +
  geom_col(
    width = 0.72, colour = "white", linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = panel_a_totals,
    aes(Bar, Intake + 55, label = round(Intake)),
    inherit.aes = FALSE, fontface = "bold", family = base_font,
    size = geom_text_size
  ) +
  annotate("segment", x = 0.65, xend = 2.35, y = 3370, yend = 3370,
           linewidth = 0.32, colour = "#666666") +
  annotate("segment", x = 2.65, xend = 4.35, y = 3370, yend = 3370,
           linewidth = 0.32, colour = "#666666") +
  annotate("text", x = 1.5, y = 3470, label = "Female",
           fontface = "bold", family = base_font, size = geom_text_size) +
  annotate("text", x = 3.5, y = 3470, label = "Male",
           fontface = "bold", family = base_font, size = geom_text_size) +
  scale_fill_manual(
    values = food_cols,
    breaks = food_levels,
    labels = food_labels[food_levels],
    name = NULL
  ) +
  scale_x_discrete(
    labels = c(
      "FML__Current diets" = "Current",
      "FML__Optimized diets" = "Optimized",
      "MLE__Current diets" = "Current",
      "MLE__Optimized diets" = "Optimized"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 3520), breaks = seq(0, 3000, 500),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Energy intake (kcal/cap/day)",
    title = NULL
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_classic(base_size = text_size_pt, base_family = base_font) +
  theme(
    plot.title = element_text(face = "bold", size = text_size_pt, hjust = 0,
                              margin = margin(b = 0)),
    axis.title = element_text(size = 12, colour = "#000000"),
    axis.title.y = element_text(size = 12, colour = "#000000", margin = margin(r = 4)),
    axis.text = element_text(size = 12, colour = "#000000"),
    axis.text.x = element_text(size = 12, colour = "#000000", lineheight = 0.9),
    axis.ticks.x = element_blank(),
    axis.line = element_line(linewidth = 0.38, colour = "black"),
    panel.grid.major.y = element_line(colour = "#D9D9D9", linewidth = 0.27),
    legend.position = "bottom",
    legend.text = element_text(size = text_size_pt),
    legend.key.height = unit(1.6, "mm"),
    legend.key.width = unit(2.4, "mm"),
    legend.spacing.x = unit(2.5, "mm"),
    legend.spacing.y = unit(0, "mm"),
    legend.box.spacing = unit(5.5, "mm"),
    plot.margin = margin(2, 1, 0, 1)
  )

# -----------------------------------------------------------------------------
# Panel b: food-group-specific changes by income group
# -----------------------------------------------------------------------------
panel_b_data <- changes |>
  filter(
    Age == "All", region %in% c(income_levels,"World"),
    Sex %in% c("FML", "MLE"), FoodGroup %in% food_levels
  ) |>
  mutate(
    region = factor(region, levels = c(income_levels,"World")),
    FoodGroup = factor(FoodGroup, levels = food_levels),
    Sex = factor(Sex, levels = c("FML", "MLE"), labels = c("Female", "Male")),
    region_id = as.integer(region),
    food_id = as.integer(FoodGroup),
    y_num = (region_id - 1) * 9.35 + food_id
  ) |>
  group_by(region, FoodGroup) |>
  mutate(
    pct_label = sprintf("%.0f%%", pct_change),
    # Put both sex-specific values outside the connecting segment. If the two
    # values are identical, use sex as a stable tie-breaker.
    label_hjust = case_when(
      pct_change < max(pct_change) ~ 1.25,
      pct_change > min(pct_change) ~ -0.25,
      Sex == "Female" ~ 1.25,
      TRUE ~ -0.25
    )
  ) |>
  ungroup()

panel_b_lines <- panel_b_data |>
  select(region, FoodGroup, y_num, Sex, pct_change) |>
  pivot_wider(names_from = Sex, values_from = pct_change) |>
  mutate(
    xmin = pmin(Female, Male), xmax = pmax(Female, Male),
    direction = if_else((Female + Male) / 2 >= 0, "Increase", "Reduction")
  )

b_y_breaks <- panel_b_data |>
  distinct(region, FoodGroup, y_num) |>
  arrange(y_num)

region_centres <- panel_b_data |>
  group_by(region) |>
  summarise(y = mean(range(y_num)), .groups = "drop")

shade_rects <- tibble(
  xmin = -105, xmax = 365,
  ymin = c(9.675, 28.375), ymax = c(18.025, 36.725)
)

panel_b_plot <- ggplot() +
  geom_rect(
    data = shade_rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#F2F2F1", colour = NA
  ) +
  geom_hline(yintercept = c(9.175, 18.525, 27.875),
             linewidth = 0.28, colour = "#BEBEBE") +
  geom_segment(
    data = panel_b_lines,
    aes(x = xmin, xend = xmax, y = y_num, yend = y_num, colour = direction),
    linewidth = 0.75, lineend = "round"
  ) +
  geom_point(
    data = panel_b_data,
    aes(x = pct_change, y = y_num, shape = Sex),
    size = 2, stroke = 0.45, colour = "#252525", fill = "white"
  ) +
  geom_point(
    data = filter(panel_b_data, Sex == "Male"),
    aes(x = pct_change, y = y_num),
    shape = 22, size = 2, stroke = 0.25,
    colour = "white", fill = "#252525"
  ) +
  geom_text(
    data = filter(panel_b_data, Sex == "Female"),
    aes(
      x = pct_change, y = y_num, label = pct_label,
      hjust = label_hjust
    ),
    family = base_font, size = 10 / ggplot2::.pt,
    fontface = "bold", colour = "#1F77B4", vjust = 0.45
  ) +
  geom_text(
    data = filter(panel_b_data, Sex == "Male"),
    aes(
      x = pct_change, y = y_num, label = pct_label,
      hjust = label_hjust
    ),
    family = base_font, size = 10 / ggplot2::.pt,
    fontface = "bold", colour = "#D62728", vjust = 0.45
  ) +
  geom_vline(xintercept = 0, linewidth = 0.42, colour = "#444444") +
  annotate(
    "text", x = -213, y = region_centres$y,
    label = income_labels[as.character(region_centres$region)],
    angle = 90, hjust = 0.50, vjust = 0.50,
    fontface = "bold", family = base_font,
    size = geom_text_size
  ) +
  scale_colour_manual(
    values = c("Increase" = positive_color, "Reduction" = negative_color),
    breaks = c("Increase", "Reduction"), name = NULL
  ) +
  scale_shape_manual(
    values = c("Female" = 21, "Male" = 22),
    breaks = c("Female", "Male"), name = NULL
  ) +
  scale_x_continuous(
    breaks = c(-100, 0, 100, 200, 300), expand = expansion(mult = c(0, 0))
  ) +
  scale_y_reverse(
    breaks = b_y_breaks$y_num,
    labels = food_labels[as.character(b_y_breaks$FoodGroup)],
    # Leave enough room for the enlarged markers in the first and last rows.
    # With 0.55 row units, High income / Legumes & nuts was clipped at the
    # upper panel boundary after the point size was increased to 2 mm.
    expand = expansion(add = c(1.15, 0.80))
  ) +
  coord_cartesian(xlim = c(-105, 365), clip = "off") +
  labs(
    x = "Change from current diets (%)", y = NULL,
    title = "Food-group-specific dietary changes"
  ) +
  guides(
    shape = guide_legend(
      order = 1,
      override.aes = list(
        fill = c("white", "#252525"), colour = "#252525",
        size = 1.50, stroke = 0.45
      )
    ),
    colour = guide_legend(order = 2, override.aes = list(linewidth = 0.80))
  ) +
  theme_classic(base_size = text_size_pt, base_family = base_font) +
  theme(
    plot.title = element_text(face = "bold", size = text_size_pt, hjust = 0,
                              margin = margin(b = 4)),
    axis.title = element_text(size = 12, colour = "#000000"),
    axis.title.x = element_text(size = 12, colour = "#000000", margin = margin(t = 3)),
    axis.text.x = element_text(size = 12, colour = "#000000"),
    axis.text.y = element_text(size = 12, colour = "#000000",
                               margin = margin(r = 14)),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.38, colour = "#000000"),
    panel.grid.major.x = element_line(colour = "#DADADA", linewidth = 0.28),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "right",
    legend.text = element_text(size = text_size_pt),
    legend.key.width = unit(3.4, "mm"),
    legend.spacing.x = unit(1.0, "mm"),
    plot.margin = margin(0, 2, 1, 49)
  )

# Open the final SVG device before extracting legends. This makes grid use
# svglite/systemfonts for Arial metrics instead of the PostScript font database.
svg_file <- file.path(output_dir, "Fig1new.svg")
svglite::svglite(
  svg_file,
  width = figure_width_mm / 25.4,
  height = figure_height_mm / 25.4,
  bg = "white"
)

panel_b_legend <- cowplot::get_legend(
  panel_b_plot +
    labs(title = NULL) +
    theme(
      legend.position = "top",
      plot.margin = margin(0, 0, 0, 0)
    )
)

panel_b_core <- panel_b_plot +
  labs(title = NULL) +
  theme(
    legend.position = "none",
    # The title and legend are drawn in a dedicated header band below, so the
    # core plot only needs a small top margin.
    plot.margin = margin(2, 2, 1, 49)
  )

# Reserve the upper 11% of panel b for its title and legend.  Previously the
# core plot extended to 95% while the legend occupied 89.5%-99.5%, causing the
# first food-group row to sit underneath the legend background.
panel_b <- ggdraw() +
  draw_plot(panel_b_core, x = 0, y = 0, width = 1, height = 0.91) +
  draw_grob(panel_b_legend, x = 0.330, y = 0.920, width = 0.340, height = 0.070)
# -----------------------------------------------------------------------------
# Panel c: age-specific bubble matrices
# -----------------------------------------------------------------------------
panel_c_data <- changes |>
  filter(
    region == "World", Age %in% age_levels, Sex %in% c("FML", "MLE"),
    FoodGroup %in% food_levels
  ) |>
  mutate(
    Age = factor(Age, levels = rev(age_levels)),
    FoodGroup = factor(FoodGroup, levels = food_levels),
    Sex = factor(Sex, levels = c("FML", "MLE"), labels = c("Female", "Male")),
    bubble_size = 0.27 * sqrt(18 + pmin(abs(pct_change), 350) / 350 * 185)
  )

fill_scale <- scale_fill_gradientn(
  colours = c("#A9463E", "#E3B7AF", "#FAFAF8", "#CFE4DC", "#2F806A"),
  values = scales::rescale(c(-100, -50, 0, 175, 350), from = c(-100, 350)),
  limits = c(-100, 350), oob = squish,
  breaks = seq(-100, 350, 50),
  name = "Change from current diets (%)"
)

size_scale <- scale_size_identity(
  guide = "legend",
  breaks = 0.27 * sqrt(18 + c(50, 150, 300) / 350 * 185),
  labels = c("50%", "150%", "300%"),
  name = "Magnitude"
)

panel_c_base <- ggplot(panel_c_data, aes(FoodGroup, Age)) +
  geom_point(
    aes(size = bubble_size, fill = pct_change), shape = 21,
    colour = "#818181", stroke = 0.28
  ) +
  facet_wrap(~Sex, nrow = 1) +
  scale_x_discrete(labels = food_labels_c, expand = expansion(add = 0.48)) +
  scale_y_discrete(expand = expansion(add = 0.40)) +
  size_scale + fill_scale +
  labs(x = NULL, y = "Age groups") +
  theme_minimal(base_size = text_size_pt, base_family = base_font) +
  theme(
    panel.grid.major = element_line(colour = "#E2E2E2", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = 12, angle = 90, hjust = 1, vjust = 0.5,
      colour = "#000000"
    ),
    axis.text.y = element_text(size = 12, colour = "#000000", margin = margin(r = 0)),
    axis.title = element_text(size = 12, colour = "#000000"),
    axis.title.y = element_text(size = 12, colour = "#000000", margin = margin(r = 0)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = text_size_pt, hjust = 0.5),
    panel.spacing.x = unit(5.0, "mm"),
    plot.margin = margin(0, 0, 0, 0)
  )

panel_c_matrix <- panel_c_base + theme(legend.position = "none")

colour_legend <- cowplot::get_legend(
  panel_c_base +
    guides(
      size = "none",
      fill = guide_colourbar(
        title.position = "right", barheight = unit(47, "mm"),
        barwidth = unit(3.3, "mm"), ticks = TRUE,
        frame.colour = "black", frame.linewidth = 0.3
      )
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = text_size_pt, angle = 90, vjust = 0.5),
      legend.text = element_text(size = text_size_pt),
      legend.spacing.x = unit(1.0, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

magnitude_data <- tibble(
  x = c(0.35, 2.55, 4.95),
  label_x = c(0.85, 3.05, 5.45),
  label = c("50%", "150%", "300%"),
  bubble_size = 1.40 * 0.27 * sqrt(18 + c(50, 150, 300) / 350 * 185)
)

magnitude_legend <- ggplot(magnitude_data, aes(x, 0.50)) +
  geom_point(
    aes(size = bubble_size), shape = 21, fill = "white",
    colour = "black", stroke = 0.28
  ) +
  geom_text(
    aes(label_x, 0.50, label = label), hjust = 0,
    family = base_font, size = geom_text_size
  ) +
  scale_size_identity() +
  coord_cartesian(xlim = c(0, 6.5), ylim = c(0, 1), clip = "off") +
  theme_void(base_size = text_size_pt, base_family = base_font)

# Complete the shared legend row above panel b: sex, direction, then the three
# percentage-magnitude examples, matching the Adobe-edited layout.
panel_b <- panel_b +
  draw_plot(magnitude_legend, x = 0.660, y = 0.920, width = 0.320, height = 0.070)

panel_c <- ggdraw() +
  draw_plot(panel_c_matrix, x = 0.05, y = 0.00, width = 0.82, height = 0.98) +
  draw_grob(colour_legend, x = 0.934, y = 0.18, width = 0.066, height = 0.78) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# -----------------------------------------------------------------------------
# Assemble at the requested 230 mm width. The extra page height is assigned to
# panel b so its 32 food-group rows have more vertical separation; panels a and
# c retain their previous physical dimensions.
# -----------------------------------------------------------------------------
full_figure <- ggdraw() +
  draw_plot(panel_a, x = 0.035, y = 0.604, width = 0.388, height = 0.369) +
  draw_plot(panel_c, x = 0.435, y = 0.604, width = 0.535, height = 0.378) +
  draw_plot(panel_b, x = 0.020, y = 0.0045, width = 0.950, height = 0.5905) +
  draw_label("a", x = 0.043, y = 0.978, hjust = 0, vjust = 1,
             fontface = "bold", fontfamily = base_font, size = text_size_pt) +
  draw_label("c", x = 0.449, y = 0.978, hjust = 0, vjust = 1,
             fontface = "bold", fontfamily = base_font, size = text_size_pt) +
  draw_label("b", x = 0.043, y = 0.582, hjust = 0, vjust = 1,
             fontface = "bold", fontfamily = base_font, size = text_size_pt) +
  theme(plot.background = element_rect(fill = NA, colour = NA))

print(full_figure)
invisible(grDevices::dev.off())

message("Saved: ", normalizePath(svg_file))
library(openxlsx)
data_fig1 <- list()
data_fig1[['fig1a']] <- panel_a_data
data_fig1[['fig1b']] <- panel_b_data
data_fig1[['fig1c']] <- panel_c_data
write.xlsx(data_fig1,'fig1_plot.xlsx')
