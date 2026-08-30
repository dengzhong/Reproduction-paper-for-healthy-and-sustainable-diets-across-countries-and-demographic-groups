library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

base_dir <- getwd()

deaths_path <- file.path(base_dir, "health_food_deaths_step3.xlsx")
income_path <- file.path(base_dir, "income group.xlsx")

food_group_levels <- c("Fruits", "Vegetables", "Nuts", "Legumes", "Red meat")
food_group_labels <- c(
  "Fruits" = "Fruits",
  "Vegetables" = "Vegetables",
  "Nuts" = "Nuts",
  "Legumes" = "Legumes",
  "Red meat" = "Redmeat"
)
endpoint_levels <- c(
  "CHD",
  "Stroke",
  "Cancer",
  "T2D"
)

endpoint_palette <- c(
  "CHD" = "#2F5D8A",
  "Stroke" = "#D96B4E",
  "Cancer" = "#7FA35A",
  "T2D" = "#4C9A9A"
)

#----step one-----
# read deaths and income group data
deaths_df <- read_excel(deaths_path, sheet = "delta_deaths_detail", .name_repair = "minimal")
income_df <- read_excel(income_path, .name_repair = "minimal") %>%
  rename(income_code = `Income-level`)

#----step two-----
# prepare income groups and disease categories
plot_df <- deaths_df %>%
  mutate(
    Endpoint = case_when(
      Endpoint == "Ischemic heart disease" ~ "CHD",
      Endpoint == "Diabetes mellitus type 2" ~ "T2D",
      grepl("cancer", Endpoint, ignore.case = TRUE) ~ "Cancer",
      TRUE ~ Endpoint
    ),
    `Food group` = factor(`Food group`, levels = food_group_levels),
    Endpoint = factor(Endpoint, levels = endpoint_levels)
  ) %>%
  left_join(income_df, by = "ISO3") %>%
  filter(!is.na(income_code)) %>%
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
# sum deaths across age groups and countries
income_food_endpoint_df <- plot_df %>%
  group_by(income_group, `Food group`, Endpoint) %>%
  summarise(delta_deaths = sum(delta_deaths, na.rm = TRUE), .groups = "drop")

global_food_endpoint_df <- plot_df %>%
  group_by(`Food group`, Endpoint) %>%
  summarise(delta_deaths = sum(delta_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_group = "Global")

plot_sum_df <- bind_rows(global_food_endpoint_df, income_food_endpoint_df) %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c("Global", "High", "Upper-middle", "Low-middle", "Low")
    ),
    `Food group` = factor(`Food group`, levels = food_group_levels),
    Endpoint = factor(Endpoint, levels = endpoint_levels),
    delta_deaths_1000 = delta_deaths / 1000
  ) %>%
  filter(!is.na(`Food group`), !is.na(Endpoint))

label_df <- plot_sum_df %>%
  group_by(income_group, `Food group`) %>%
  summarise(
    total_delta_deaths_1000 = sum(delta_deaths_1000, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(income_group) %>%
  mutate(
    panel_max = max(total_delta_deaths_1000, na.rm = TRUE),
    panel_min = min(total_delta_deaths_1000, na.rm = TRUE),
    offset = pmax(abs(panel_max - panel_min) * 0.04, 8),
    label_y = ifelse(
      total_delta_deaths_1000 >= 0,
      total_delta_deaths_1000 + offset,
      total_delta_deaths_1000 - offset
    ),
    label_vjust = ifelse(total_delta_deaths_1000 >= 0, 0, 1)
  ) %>%
  ungroup()

#----step four-----
# draw figure
fig4b_plot <- ggplot(plot_sum_df, aes(x = `Food group`, y = delta_deaths_1000, fill = Endpoint)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.12) +
  geom_text(
    data = label_df,
    aes(x = `Food group`, y = label_y, label = round(total_delta_deaths_1000, 0), vjust = label_vjust),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(~ income_group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = endpoint_palette, drop = FALSE) +
  scale_x_discrete(labels = food_group_labels) +
  scale_y_continuous(
    expand = expansion(mult = c(0.12, 0.25))
  ) +
  labs(
    x = NULL,
    y = "Change in mortality (1000 persons)",
    fill = "Cause of death"
  ) +
  coord_cartesian(clip = "off") +
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
    plot.margin = margin(18, 14, 18, 10)
  )

print(fig4b_plot)
ggsave(
  filename = 'Fig4b.svg',
  plot = fig4b_plot,
  width = 95,
  height = 290,
  units = "mm",
  device = svglite::svglite,
  bg = "white"
)
