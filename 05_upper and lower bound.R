library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)

# Paths -------------------------------------------------------------------

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
input_dir <- file.path(project_dir, "Input data")
output_dir <- file.path(project_dir, "output data")

sex_files <- c(
  FML = file.path(output_dir, "FML_age_groups.xlsx"),
  MLE = file.path(output_dir, "MLE_age_groups.xlsx")
)

country_path <- file.path(input_dir, "Country.xlsx")
food_path <- file.path(input_dir, "Food.xlsx")


# Restricted meat rules --------------------------------------------------

pork_foods <- "pork"
beef_foods <- c("beef", "lamb")

pork_restricted <- c(
  "ARE", "BGD", "DJI", "DZA", "EGY", "GIN", "GMB", "IDN", "IRN",
  "IRQ", "JOR", "KWT", "MAR", "MDV", "MLI", "MRT", "MYS", "NER",
  "PAK", "SAU", "SDN", "SEN", "TJK", "TUN", "UZB", "YEM"
)

beef_restricted <- c("IND", "NPL")


# Target country and food order ------------------------------------------

read_order <- function(path, column) {
  values <- read_excel(path)[[column]] |>
    as.character() |>
    trimws()

  unique(values[!is.na(values) & values != ""])
}

country_order <- read_order(country_path, "ISO3")
food_order <- read_order(food_path, "FoodItem")


# Data preparation -------------------------------------------------------

read_age_sheets <- function(path) {
  sheet_names <- excel_sheets(path)

  data <- map_dfr(sheet_names, function(sheet) {
    age_data <- read_excel(path, sheet = sheet)
    missing_columns <- setdiff(c("ISO3", food_order), names(age_data))

    if (length(missing_columns) > 0) {
      stop(
        "Sheet '", sheet, "' is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      )
    }

    age_data |>
      mutate(ISO3 = trimws(as.character(ISO3))) |>
      filter(ISO3 %in% country_order) |>
      select(ISO3, all_of(food_order)) |>
      pivot_longer(
        cols = all_of(food_order),
        names_to = "Food",
        values_to = "Value"
      ) |>
      transmute(
        AgeGroup = sheet,
        ISO3,
        Food = as.character(Food),
        Value = as.numeric(Value)
      )
  })

  list(data = data, sheets = sheet_names)
}

calculate_bounds <- function(data) {
  upper_limits <- data |>
    group_by(AgeGroup, Food) |>
    summarise(
      Upper_raw = quantile(Value, probs = 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  data |>
    left_join(upper_limits, by = c("AgeGroup", "Food")) |>
    mutate(
      restricted =
        (ISO3 %in% pork_restricted & Food %in% pork_foods) |
        (ISO3 %in% beef_restricted & Food %in% beef_foods),
      Lower = Value * 0.1,
      Upper = if_else(
        restricted,
        Value,
        pmax(Upper_raw, Value, na.rm = TRUE)
      )
    ) |>
    select(AgeGroup, ISO3, Food, Lower, Upper)
}

make_wide_sheets <- function(data, value_column, sheet_names) {
  set_names(sheet_names) |>
    map(function(sheet) {
      data |>
        filter(AgeGroup == sheet) |>
        transmute(ISO3, Food, Value = .data[[value_column]]) |>
        mutate(
          ISO3 = factor(ISO3, levels = country_order),
          Food = factor(Food, levels = food_order)
        ) |>
        pivot_wider(
          names_from = Food,
          values_from = Value,
          values_fill = 0,
          names_expand = TRUE
        ) |>
        arrange(ISO3) |>
        mutate(ISO3 = as.character(ISO3)) |>
        select(ISO3, all_of(food_order))
    })
}


# Process both sexes -----------------------------------------------------

process_sex <- function(sex, path) {
  input <- read_age_sheets(path)
  bounds <- calculate_bounds(input$data)

  output_paths <- c(
    upper = file.path(output_dir, paste0("food_intake_upper_by_age_", sex, ".xlsx")),
    lower = file.path(output_dir, paste0("food_intake_lower_by_age_", sex, ".xlsx"))
  )

  write_xlsx(
    make_wide_sheets(bounds, "Upper", input$sheets),
    output_paths[["upper"]]
  )
  write_xlsx(
    make_wide_sheets(bounds, "Lower", input$sheets),
    output_paths[["lower"]]
  )

  message("Wrote: ", output_paths[["upper"]])
  message("Wrote: ", output_paths[["lower"]])
}

iwalk(sex_files, ~ process_sex(.y, .x))
