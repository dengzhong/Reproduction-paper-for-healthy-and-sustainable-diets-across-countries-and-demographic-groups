library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

base_dir <- getwd()

deaths_path <- file.path(base_dir, "health_food_deaths_step3.xlsx")
income_path <- file.path(base_dir, "income group.xlsx")

adult_age_levels <- c(
  "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

food_group_levels <- c("Red meat", "Fruits", "Vegetables", "Legumes", "Nuts")

#----step one-----
# read deaths and income group data
deaths_df <- read_excel(deaths_path, sheet = "delta_deaths_detail", .name_repair = "minimal")
income_df <- read_excel(income_path, .name_repair = "minimal") %>%
  rename(income_code = `Income-level`)

#----step two-----
# prepare age group and income group
plot_df <- deaths_df %>%
  mutate(
    age_group = gsub("years", "", age_group, fixed = TRUE),
    age_group = trimws(age_group),
    age_group = factor(age_group, levels = adult_age_levels),
    `Food group` = factor(`Food group`, levels = food_group_levels)
  ) %>%
  left_join(income_df, by = "ISO3") %>%
  filter(!is.na(income_code))

plot_df <- plot_df %>%
  mutate(
    income_group = case_when(
      income_code == "H" ~ "High",
      income_code == "UM" ~ "Upper-middle",
      income_code == "LM" ~ "Low-middle",
      income_code == "L" ~ "Low",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_group))

#----step three-----
# sum deaths across diseases and countries
income_age_food_df <- plot_df %>%
  group_by(income_group, age_group, `Food group`) %>%
  summarise(delta_deaths = sum(delta_deaths, na.rm = TRUE), .groups = "drop")

global_age_food_df <- plot_df %>%
  group_by(age_group, `Food group`) %>%
  summarise(delta_deaths = sum(delta_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_group = "Global")

heatmap_df <- bind_rows(global_age_food_df, income_age_food_df) %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c("Global", "High", "Upper-middle", "Low-middle", "Low")
    ),
    age_group = factor(age_group, levels = adult_age_levels),
    `Food group` = factor(`Food group`, levels = rev(food_group_levels)),
    deaths_1000 = delta_deaths / 1000
  ) %>%
  filter(!is.na(age_group), !is.na(`Food group`))

#----step four-----
# draw heatmap
fig4b_heatmap <- ggplot(heatmap_df, aes(x = age_group, y = `Food group`, fill = deaths_1000)) +
  geom_tile(color = "white", linewidth = 0.6) +
  facet_wrap(~ income_group, ncol = 1, strip.position = "top", scales = "free_y") +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    values = rescale(c(0, 0.1, 0.35, 0.7, 1)),
    name = "Deaths\n(1,000 persons)"
  ) +
  labs(
    x = "Age group",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "#222222"),
    axis.text.y = element_text(color = "#222222", face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 18, face = "bold", color = "#111111"),
    strip.background = element_rect(fill = "#f2f2f2", color = "#bdbdbd", linewidth = 0.8),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    panel.spacing.y = unit(0.8, "lines"),
    plot.margin = margin(10, 18, 10, 10)
  )

print(fig4b_heatmap)
