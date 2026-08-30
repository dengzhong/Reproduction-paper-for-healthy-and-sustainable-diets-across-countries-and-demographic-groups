library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Convert supply data to intake data
rm(list = ls())

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
input_dir <- file.path(project_dir, "Input data")
output_dir <- file.path(project_dir, "output data")

read_filter_values <- function(file) {
  values <- read_excel(file) |>
    pull(1) |>
    as.character() |>
    str_trim()

  unique(values[!is.na(values) & values != ""])
}

country_order <- read_filter_values(file.path(input_dir, "Country.xlsx"))
food_order <- read_filter_values(file.path(input_dir, "Food.xlsx"))

food_map <- read_excel(file.path(input_dir, "FoodItem to Food.xlsx")) |>
  transmute(
    FoodItem = str_trim(as.character(FoodItem)),
    Food = str_trim(as.character(Food))
  ) |>
  filter(
    !is.na(FoodItem), FoodItem != "",
    !is.na(Food), Food != ""
  ) |>
  distinct(FoodItem, .keep_all = TRUE)

use_short_food_names <- function(data) {
  data |>
    inner_join(food_map, by = "FoodItem") |>
    select(-FoodItem) |>
    rename(FoodItem = Food)
}

food_kcal <- read_excel(file.path(input_dir, "FBS_kcal.xlsx")) |>
  use_short_food_names()

waste_CF <- read_excel(file.path(input_dir, "waste_conversion factors.xlsx")) |>
  use_short_food_names() |>
  pivot_longer(
    cols = -c(FoodItem),
    names_to = "Utilisation",
    values_to = "CF"
  )

waste_food <- read_excel(file.path(input_dir, "waste_food.xlsx")) %>%
  pivot_longer(
    cols = -c(ISO3, Stage, Utilisation),
    names_to = "FoodItem",
    values_to = "Food_waste"
  ) %>%
  use_short_food_names() %>%
  pivot_wider(
    id_cols = c(ISO3, FoodItem, Utilisation),
    names_from = Stage,
    values_from = Food_waste,
    names_glue = "{Stage}_waste",
    values_fill = 0
  )

waste_proc <- read_excel(file.path(input_dir, "Share of processed food.xlsx")) %>%
  use_short_food_names() %>%
  pivot_longer(
    cols = -c(ISO3, FoodItem),
    names_to = "Utilisation",
    values_to = "Share"
  )

food_kcal_w <- food_kcal |>
  left_join(waste_CF, by = "FoodItem") |>
  left_join(
    waste_food,
    by = c("ISO3", "FoodItem", "Utilisation")
  ) |>
  left_join(
    waste_proc,
    by = c("ISO3", "FoodItem", "Utilisation")
  )

waste_factor_long <- food_kcal_w |>
  filter(
    ISO3 %in% country_order,
    FoodItem %in% food_order,
    Utilisation %in% c("fresh", "processed")
  ) |>
  mutate(
    value = Share / 100 * CF *
      (1 - distribution_waste / 100) *
      (1 - consumption_waste / 100)
  ) |>
  group_by(ISO3, FoodItem) |>
  summarise(value = sum(value), .groups = "drop")

waste_factor <- expand_grid(
  ISO3 = country_order,
  FoodItem = food_order
) |>
  left_join(waste_factor_long, by = c("ISO3", "FoodItem")) |>
  pivot_wider(
    names_from = FoodItem,
    values_from = value
  ) |>
  select(ISO3, all_of(food_order))

write.xlsx(
  waste_factor,
  file.path(output_dir, "waste_factor.xlsx"),
  sheetName = "Sheet1",
  rowNames = FALSE,
  overwrite = TRUE
)
