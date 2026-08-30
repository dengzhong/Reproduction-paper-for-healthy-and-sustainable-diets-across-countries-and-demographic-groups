library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(openxlsx)

# Paths -------------------------------------------------------------------

data_dir <- "~/Desktop/dengzc/2026/paper/national age specific/Code/"

fbs_path <- file.path(
  data_dir,
  "Input data/FoodBalanceSheets.xlsx"
)
item_path <- file.path(data_dir, "Input data/Fooditem.xlsx")
output_path <- file.path(data_dir, "food_proxy_code_data_reps/input_reps_full/FBS_intake_data.xlsx")


# Constants ---------------------------------------------------------------

unit_levels <- c("kcal/d", "kcal/d_w", "g/d", "g/d_w")
summary_items <- c("all-fg", "total")


# Functions ---------------------------------------------------------------

prepare_supply <- function(data, food_items, element, unit, scale = 1,
                           zero_items = character()) {
  data |>
    filter(Element == element, FoodItem %in% food_items) |>
    group_by(ISO3, Year, FoodItem) |>
    summarise(Value = first(Value), .groups = "drop") |>
    complete(
      ISO3,
      Year,
      FoodItem = food_items,
      fill = list(Value = 0)
    ) |>
    mutate(
      FoodItem = as.character(FoodItem),
      Unit = unit,
      Value = if_else(FoodItem %in% zero_items, 0, Value * scale)
    ) |>
    select(ISO3, Year, FoodItem, Unit, Value)
}

apply_waste_adjustment <- function(data, adjusted_unit, waste_cf,
                                   waste_food, waste_processed) {
  data |>
    left_join(waste_cf, by = "FoodItem") |>
    left_join(
      waste_food,
      by = c("ISO3", "FoodItem", "Utilisation")
    ) |>
    left_join(
      waste_processed,
      by = c("ISO3", "FoodItem", "Utilisation")
    ) |>
    mutate(
      Value = Value * (Share / 100) * CF *
        (1 - distribution_waste / 100) *
        (1 - consumption_waste / 100)
    ) |>
    group_by(ISO3, Year, FoodItem) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
    mutate(Unit = adjusted_unit) |>
    select(ISO3, Year, FoodItem, Unit, Value)
}

# Read and reshape source data --------------------------------------------

food_items <- read_excel(item_path) |>
  pull(1) |>
  as.character() |>
  str_trim()

food_items <- food_items[!is.na(food_items) & food_items != ""]

fbs <- read_excel(fbs_path, sheet = "FBS_ISO_filtered") |>
  select(ISO3, FoodItem = Item, Element, starts_with("Y")) |>
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "Value"
  ) |>
  mutate(
    Year = as.integer(str_remove(Year, "^Y")),
    Value = replace_na(as.numeric(Value), 0)
  )

waste_cf <- read_excel(
  file.path(data_dir, "Input data/waste_conversion factors.xlsx")
) |>
  pivot_longer(
    cols = -FoodItem,
    names_to = "Utilisation",
    values_to = "CF"
  )

waste_food <- read_excel(file.path(data_dir, "Input data/waste_food.xlsx")) |>
  pivot_longer(
    cols = -c(ISO3, Stage, Utilisation),
    names_to = "FoodItem",
    values_to = "Food_waste"
  ) |>
  pivot_wider(
    id_cols = c(ISO3, FoodItem, Utilisation),
    names_from = Stage,
    values_from = Food_waste,
    names_glue = "{Stage}_waste",
    values_fill = 0
  )

waste_processed <- read_excel(
  file.path(data_dir, "Input data/Share of processed food.xlsx")
) |>
  pivot_longer(
    cols = -c(ISO3, FoodItem),
    names_to = "Utilisation",
    values_to = "Share"
  )


# Build model input -------------------------------------------------------

food_kcal <- prepare_supply(
  data = fbs,
  food_items = food_items,
  element = "Food supply (kcal/capita/day)",
  unit = "kcal/d",
  zero_items = "Alcohol, Non-Food"
)

food_weight <- prepare_supply(
  data = fbs,
  food_items = food_items,
  element = "Food supply quantity (kg/capita/yr)",
  unit = "g/d",
  scale = 1000 / 365
)

write.xlsx(food_kcal, '~/Desktop/dengzc/2026/paper/national age specific/Code/Input data/FBS_kcal.xlsx')
write.xlsx(food_weight, '~/Desktop/dengzc/2026/paper/national age specific/Code/Input data/FBS_weight.xlsx')

food_kcal_input <- bind_rows(
  food_kcal,
  apply_waste_adjustment(
    food_kcal,
    adjusted_unit = "kcal/d_w",
    waste_cf = waste_cf,
    waste_food = waste_food,
    waste_processed = waste_processed
  )
)

food_weight_input <- bind_rows(
  food_weight,
  apply_waste_adjustment(
    food_weight,
    adjusted_unit = "g/d_w",
    waste_cf = waste_cf,
    waste_food = waste_food,
    waste_processed = waste_processed
  )
)

food_item_levels <- unique(c(summary_items, food_items))

# Preserve FBS_input as the original wide table.
FBS_input <- bind_rows(food_kcal_input, food_weight_input) |>
  mutate(
    FoodItem = factor(FoodItem, levels = food_item_levels),
    Unit = factor(Unit, levels = unit_levels)
  ) |>
  arrange(ISO3, FoodItem, Year, Unit) |>
  pivot_wider(
    id_cols = c(ISO3, Year, FoodItem),
    names_from = Unit,
    values_from = Value,
    values_fill = list(Value = 0),
    values_fn = list(Value = sum)
  )

# Create the final long table without changing FBS_input.
FBS_input_long <- FBS_input |>
  # Retain only country-food combinations available for all four measures.
  filter(if_all(all_of(unit_levels), ~ .x != 0)) |>
  mutate(FoodItem = factor(FoodItem, levels = food_item_levels)) |>
  arrange(ISO3, FoodItem, Year) |>
  pivot_longer(
    cols = all_of(unit_levels),
    names_to = "Unit",
    values_to = "Value"
  ) |>
  mutate(Unit = factor(Unit, levels = unit_levels))

FBS_input_detail <- FBS_input_long |>
  filter(!as.character(FoodItem) %in% summary_items)

kcal_summaries <- FBS_input_detail |>
  filter(Unit %in% c("kcal/d", "kcal/d_w")) |>
  group_by(ISO3, Year, Unit) |>
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
  crossing(FoodItem = summary_items) |>
  select(ISO3, Year, FoodItem, Unit, Value)

FBS_input_final <- bind_rows(FBS_input_detail, kcal_summaries) |>
  mutate(
    FoodItem = factor(FoodItem, levels = food_item_levels),
    Unit = factor(Unit, levels = unit_levels)
  ) |>
  arrange(ISO3, Year, FoodItem, Unit) |> 
  rename(rgs = ISO3,
         fooditem = FoodItem,
         unit = Unit,
         year = Year,
         value = Value) |> 
  select(rgs,fooditem,unit,year,value)

write.xlsx(FBS_input_final, output_path)
