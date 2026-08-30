required_packages <- c("readxl", "dplyr", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(readxl)
library(dplyr)
library(ggplot2)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[[1]]) else "plot_fig4b_contribution_by_income_age.R"
base_dir <- dirname(normalizePath(script_path, mustWork = FALSE))

input_file <- if (basename(base_dir) == "output new intensity") {
  file.path(base_dir, "Fig4.xlsx")
} else {
  file.path(base_dir, "output new intensity", "Fig4.xlsx")
}

age_levels <- c(
  "0-4years", "5-9years", "10-14years", "15-19years",
  "20-24years", "25-29years", "30-34years", "35-39years",
  "40-44years", "45-49years", "50-54years", "55-59years",
  "60-64years", "65-69years", "70-74years", "75-79years",
  "80+years"
)

income_levels <- c("Global", "High", "Upper-middle", "Low-middle", "Low")

key_nutrients <- c("Calcium", "Folate", "VitaminA", "Magnesium", "Potassium", "Riboflavin")
nutrient_levels <- c(key_nutrients, "Other")

nutrient_colors <- setNames(
  c(
    "#8FB3D9",
    "#A8D5D2",
    "#EFD98A",
    "#D7B5D8",
    "#A6CF8C",
    "#F3B37A",
    "#E6E6E6"
  ),
  nutrient_levels
)

income_age <- read_excel(input_file, sheet = "Fig4b_summary_by_income_age") %>%
  transmute(
    `Income group`,
    `Age group`,
    Nutrient,
    Contribution = `Mean contribution share of adequacy change`
  )

global_age <- read_excel(input_file, sheet = "Fig4b_summary_by_age") %>%
  transmute(
    `Income group` = "Global",
    `Age group`,
    Nutrient,
    Contribution = `Mean contribution share of adequacy change`
  )

plot_data <- bind_rows(global_age, income_age) %>%
  filter(
    !is.na(`Income group`),
    !is.na(`Age group`),
    !is.na(Nutrient),
    !is.na(Contribution)
  ) %>%
  mutate(
    `Income group` = recode(
      `Income group`,
      "H" = "High",
      "UM" = "Upper-middle",
      "LM" = "Low-middle",
      "L" = "Low"
    ),
    Nutrient = if_else(Nutrient %in% key_nutrients, Nutrient, "Other")
  ) %>%
  group_by(`Income group`, `Age group`, Nutrient) %>%
  summarise(Contribution = sum(Contribution, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    `Income group` = factor(`Income group`, levels = income_levels),
    `Age group` = factor(`Age group`, levels = age_levels),
    Nutrient = factor(Nutrient, levels = nutrient_levels),
    Contribution_percent = Contribution * 100,
    Label = if_else(Contribution > 0.15, sprintf("%.0f", Contribution_percent), NA_character_)
  )

if (nrow(plot_data) == 0) {
  stop("No valid contribution data found in Fig4b summary sheets.", call. = FALSE)
}

p <- ggplot(
  plot_data,
  aes(x = `Age group`, y = Contribution_percent, fill = Nutrient)
) +
  geom_col(width = 0.82, color = "white", linewidth = 0.12) +
  geom_text(
    aes(label = Label),
    position = position_stack(vjust = 0.5),
    angle = 90,
    size = 3.5,
    color = "#333333",
    na.rm = TRUE
  ) +
  facet_wrap(vars(`Income group`), ncol = 1) +
  scale_fill_manual(values = nutrient_colors, drop = FALSE) +
  scale_x_discrete(labels = function(x) gsub("years", "", x)) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.25) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Age group",
    y = "Contribution share of adequacy change",
    fill = "Nutrient"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=12,color = 'black'),
    axis.text.y = element_text(size=12,color = 'black'),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.4),
    strip.text = element_text(face = "bold",size=10,color = 'black'),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.35),
    plot.margin = margin(10, 14, 10, 10)
  )

print(p)
ggsave(
  filename = 'Fig4a.svg',
  plot = p,
  width = 95,
  height = 290,
  units = "mm",
  device = svglite::svglite,
  bg = "white"
)