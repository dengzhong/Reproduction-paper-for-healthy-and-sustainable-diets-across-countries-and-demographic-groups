#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# Resolve paths from the script location so this script can be run from any folder.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) == 1L) {
  normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else {
  normalizePath("calculate_environmental_impacts.R", mustWork = TRUE)
}
data_dir <- dirname(script_path)

input_paths <- c(
  demand = file.path(data_dir, "sensitivity-EAT-Lancet-demand.xlsx"),
  intensity = file.path(data_dir, "intensity.xlsx"),
  waste = file.path(data_dir, "waste_coff.xlsx"),
  population = file.path(data_dir, "population_long.xlsx"),
  income = file.path(data_dir, "Income-level.xlsx")
)
output_path <- file.path(
  data_dir,
  "sensitivity-EAT-Lancet-environmental-impact.xlsx"
)

missing_files <- input_paths[!file.exists(input_paths)]
if (length(missing_files) > 0L) {
  stop("Missing input file(s): ", paste(missing_files, collapse = ", "))
}

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

environment_levels <- c(
  "GHG",
  "Land",
  "Freshwater",
  "Eutr.",
  "Acid."
)

environment_info <- tibble(
  Environment = environment_levels,
  Divisor = c(1e15, 1e13, 1e12, 1e12, 1e12)
)

current_wide <- read_excel(input_paths[["demand"]], sheet = "Demand_country")
eat_wide <- read_excel(input_paths[["demand"]], sheet = "Demand_country_EAT")
intensity_wide <- read_excel(input_paths[["intensity"]])
waste_wide <- read_excel(input_paths[["waste"]])
population_wide <- read_excel(input_paths[["population"]])
income <- read_excel(input_paths[["income"]])

names(current_wide)[1] <- "ISO3"
names(eat_wide)[1] <- "ISO3"
names(intensity_wide)[1] <- "Environment"
names(waste_wide)[1] <- "ISO3"
names(population_wide)[1:2] <- c("ISO3", "Sex")
names(income)[1:2] <- c("ISO3", "IncomeLevel")

food_cols <- setdiff(names(current_wide), "ISO3")

assert_true(
  identical(food_cols, setdiff(names(eat_wide), "ISO3")),
  "Current and EAT-Lancet demand food columns do not match."
)
assert_true(
  identical(food_cols, setdiff(names(intensity_wide), "Environment")),
  "Demand and environmental-intensity food columns do not match."
)
assert_true(
  identical(food_cols, setdiff(names(waste_wide), "ISO3")),
  "Demand and waste-coefficient food columns do not match."
)
assert_true(
  setequal(intensity_wide$Environment, environment_levels),
  "Environmental indicators in intensity.xlsx do not match environment_levels."
)
assert_true(
  !anyDuplicated(intensity_wide$Environment),
  "Duplicate environmental indicators were found in intensity.xlsx."
)

country_sets <- list(
  current = sort(current_wide$ISO3),
  eat = sort(eat_wide$ISO3),
  waste = sort(waste_wide$ISO3),
  population = sort(unique(population_wide$ISO3)),
  income = sort(income$ISO3)
)
assert_true(
  all(vapply(country_sets[-1], identical, logical(1), country_sets[[1]])),
  "ISO3 country coverage is not identical across input workbooks."
)
assert_true(!anyDuplicated(current_wide$ISO3), "Duplicate ISO3 values in current demand.")
assert_true(!anyDuplicated(eat_wide$ISO3), "Duplicate ISO3 values in EAT-Lancet demand.")
assert_true(!anyDuplicated(waste_wide$ISO3), "Duplicate ISO3 values in waste coefficients.")
assert_true(!anyDuplicated(income$ISO3), "Duplicate ISO3 values in income mapping.")

population_value_cols <- setdiff(names(population_wide), c("ISO3", "Sex"))
assert_true(length(population_value_cols) > 0L, "No population value columns were found.")

population_country <- population_wide %>%
  mutate(
    PopulationRow = rowSums(across(all_of(population_value_cols)), na.rm = FALSE)
  ) %>%
  group_by(ISO3) %>%
  summarise(Population = sum(PopulationRow), .groups = "drop")

assert_true(!anyNA(population_country$Population), "Population contains missing values.")
assert_true(all(population_country$Population > 0), "Country population must be positive.")

to_food_long <- function(data, id_col, value_name) {
  data %>%
    pivot_longer(
      cols = all_of(food_cols),
      names_to = "Food",
      values_to = value_name
    ) %>%
    select(all_of(id_col), Food, all_of(value_name))
}

demand_long <- bind_rows(
  to_food_long(current_wide, "ISO3", "Demand") %>% mutate(Scenario = "Current"),
  to_food_long(eat_wide, "ISO3", "Demand") %>% mutate(Scenario = "EAT")
) %>%
  select(Scenario, ISO3, Food, Demand)

waste_long <- to_food_long(waste_wide, "ISO3", "WasteCoefficient")
intensity_long <- to_food_long(intensity_wide, "Environment", "Intensity")

assert_true(!anyNA(demand_long$Demand), "Demand contains missing values.")
assert_true(!anyNA(waste_long$WasteCoefficient), "Waste coefficients contain missing values.")
assert_true(!anyNA(intensity_long$Intensity), "Environmental intensity contains missing values.")
assert_true(all(demand_long$Demand >= 0), "Demand contains negative values.")
assert_true(
  all(waste_long$WasteCoefficient > 0),
  "Waste coefficients must be greater than zero because the formula divides by waste."
)
assert_true(all(intensity_long$Intensity >= 0), "Environmental intensity contains negative values.")

# User-specified accounting identity:
# demand / waste * intensity * population * 365 / indicator-specific divisor
impact_detail <- demand_long %>%
  left_join(waste_long, by = c("ISO3", "Food")) %>%
  left_join(intensity_long, by = "Food", relationship = "many-to-many") %>%
  left_join(population_country, by = "ISO3") %>%
  left_join(income, by = "ISO3") %>%
  left_join(environment_info, by = "Environment")

assert_true(
  !anyNA(impact_detail[c(
    "Demand", "WasteCoefficient", "Intensity", "Population",
    "IncomeLevel", "Divisor"
  )]),
  "Missing value after joining demand, waste, intensity, population, income, and divisor data."
)

expected_detail_rows <-
  2L * length(country_sets[[1]]) * length(food_cols) * length(environment_levels)
assert_true(
  nrow(impact_detail) == expected_detail_rows,
  paste0(
    "Unexpected detail row count after joins: ", nrow(impact_detail),
    "; expected ", expected_detail_rows, "."
  )
)

impact_detail <- impact_detail %>%
  mutate(
    Environment = factor(Environment, levels = environment_levels),
    EnvironmentalImpact =
      Demand / WasteCoefficient * Intensity * Population * 365 / Divisor
  ) %>%
  arrange(Environment, Scenario, ISO3, Food) %>%
  mutate(Environment = as.character(Environment)) %>%
  select(
    Scenario, ISO3, IncomeLevel, Population, Environment, Divisor, Food,
    Demand, WasteCoefficient, Intensity, EnvironmentalImpact
  )

country_scenario <- impact_detail %>%
  group_by(Scenario, ISO3, IncomeLevel, Population, Environment, Divisor) %>%
  summarise(EnvironmentalImpact = sum(EnvironmentalImpact), .groups = "drop") %>%
  mutate(Environment = factor(Environment, levels = environment_levels)) %>%
  arrange(Environment, Scenario, ISO3) %>%
  mutate(Environment = as.character(Environment))

make_comparison <- function(data, id_cols) {
  data %>%
    select(all_of(id_cols), Scenario, EnvironmentalImpact) %>%
    pivot_wider(
      names_from = Scenario,
      values_from = EnvironmentalImpact,
      names_glue = "{Scenario}Impact"
    ) %>%
    mutate(
      Difference = EATImpact - CurrentImpact,
      ChangePct = if_else(
        CurrentImpact == 0,
        NA_real_,
        100 * Difference / CurrentImpact
      )
    )
}

country_comparison <- make_comparison(
  country_scenario,
  c("ISO3", "IncomeLevel", "Population", "Environment", "Divisor")
) %>%
  mutate(Environment = factor(Environment, levels = environment_levels)) %>%
  arrange(Environment, ISO3) %>%
  mutate(Environment = as.character(Environment))

income_scenario <- country_scenario %>%
  group_by(IncomeLevel, Scenario, Environment, Divisor) %>%
  summarise(
    Countries = n_distinct(ISO3),
    Population = sum(Population),
    EnvironmentalImpact = sum(EnvironmentalImpact),
    .groups = "drop"
  )

income_comparison <- make_comparison(
  income_scenario,
  c("IncomeLevel", "Countries", "Population", "Environment", "Divisor")
) %>%
  mutate(
    IncomeLevel = factor(
      IncomeLevel,
      levels = c(
        "Low income", "Lower middle income",
        "Upper-middle income", "High income"
      )
    ),
    Environment = factor(Environment, levels = environment_levels)
  ) %>%
  arrange(Environment, IncomeLevel) %>%
  mutate(
    IncomeLevel = as.character(IncomeLevel),
    Environment = as.character(Environment)
  )

global_scenario <- country_scenario %>%
  group_by(Scenario, Environment, Divisor) %>%
  summarise(
    Geography = "World",
    Countries = n_distinct(ISO3),
    Population = sum(Population),
    EnvironmentalImpact = sum(EnvironmentalImpact),
    .groups = "drop"
  ) %>%
  select(Geography, Countries, Population, Scenario, Environment, Divisor, EnvironmentalImpact)

global_comparison <- make_comparison(
  global_scenario,
  c("Geography", "Countries", "Population", "Environment", "Divisor")
) %>%
  mutate(Environment = factor(Environment, levels = environment_levels)) %>%
  arrange(Environment) %>%
  mutate(Environment = as.character(Environment))

income_food <- impact_detail %>%
  group_by(IncomeLevel, Scenario, Environment, Divisor, Food) %>%
  summarise(
    Countries = n_distinct(ISO3),
    Population = sum(Population),
    EnvironmentalImpact = sum(EnvironmentalImpact),
    .groups = "drop"
  ) %>%
  group_by(IncomeLevel, Scenario, Environment) %>%
  mutate(
    ContributionPct = if (sum(EnvironmentalImpact) == 0) {
      NA_real_
    } else {
      100 * EnvironmentalImpact / sum(EnvironmentalImpact)
    }
  ) %>%
  ungroup() %>%
  mutate(Environment = factor(Environment, levels = environment_levels)) %>%
  arrange(Environment, IncomeLevel, Scenario, desc(EnvironmentalImpact), Food) %>%
  mutate(Environment = as.character(Environment))

global_food <- impact_detail %>%
  group_by(Scenario, Environment, Divisor, Food) %>%
  summarise(
    Geography = "World",
    Countries = n_distinct(ISO3),
    Population = sum(Population),
    EnvironmentalImpact = sum(EnvironmentalImpact),
    .groups = "drop"
  ) %>%
  group_by(Scenario, Environment) %>%
  mutate(
    ContributionPct = if (sum(EnvironmentalImpact) == 0) {
      NA_real_
    } else {
      100 * EnvironmentalImpact / sum(EnvironmentalImpact)
    }
  ) %>%
  ungroup() %>%
  select(
    Geography, Scenario, Environment, Divisor, Food, Countries,
    Population, EnvironmentalImpact, ContributionPct
  ) %>%
  mutate(Environment = factor(Environment, levels = environment_levels)) %>%
  arrange(Environment, Scenario, desc(EnvironmentalImpact), Food) %>%
  mutate(Environment = as.character(Environment))

formula_text <- paste0(
  "EnvironmentalImpact = Demand / WasteCoefficient * Intensity * ",
  "Population * 365 / Divisor"
)

environment_output <- environment_info %>%
  mutate(
    Formula = formula_text,
    Notes = "Divisors supplied by the user; intensity rows are matched by Environment name."
  )

qa <- tibble(
  Check = c(
    "Countries in all country-level inputs",
    "Foods in demand, waste, and intensity inputs",
    "Diet scenarios",
    "Environmental indicators",
    "Sex-age population rows",
    "Population total",
    "Expected country-food-environment detail rows",
    "Actual country-food-environment detail rows",
    "Missing source values",
    "Missing joined calculation values",
    "Waste coefficients <= 0",
    "Negative demand values",
    "Negative intensity values",
    "Calculation formula"
  ),
  Result = as.character(c(
    length(country_sets[[1]]),
    length(food_cols),
    2,
    length(environment_levels),
    nrow(population_wide),
    format(sum(population_country$Population), scientific = FALSE, trim = TRUE),
    expected_detail_rows,
    nrow(impact_detail),
    sum(
      is.na(current_wide), is.na(eat_wide), is.na(intensity_wide),
      is.na(waste_wide), is.na(population_wide), is.na(income)
    ),
    sum(is.na(impact_detail)),
    sum(waste_long$WasteCoefficient <= 0),
    sum(demand_long$Demand < 0),
    sum(intensity_long$Intensity < 0),
    formula_text
  ))
)

wb <- createWorkbook(creator = "OpenAI Codex")
header_style <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#2F5597",
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)
integer_style <- createStyle(numFmt = "#,##0")
number_style <- createStyle(numFmt = "0.000000")
percent_style <- createStyle(numFmt = "0.00")
sci_style <- createStyle(numFmt = "0.000000E+00")

write_table_sheet <- function(sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, withFilter = TRUE)
  addStyle(
    wb, sheet_name, header_style,
    rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE
  )
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
  setColWidths(
    wb, sheet_name,
    cols = which(names(data) %in% c("Formula", "Notes", "Check", "Result")),
    widths = 42
  )

  integer_cols <- which(names(data) %in% c("Countries", "Population"))
  if (length(integer_cols) > 0L && nrow(data) > 0L) {
    addStyle(
      wb, sheet_name, integer_style,
      rows = 2:(nrow(data) + 1L), cols = integer_cols, gridExpand = TRUE
    )
  }
  number_cols <- which(names(data) %in% c("Demand", "WasteCoefficient", "Intensity"))
  if (length(number_cols) > 0L && nrow(data) > 0L) {
    addStyle(
      wb, sheet_name, number_style,
      rows = 2:(nrow(data) + 1L), cols = number_cols, gridExpand = TRUE
    )
  }
  impact_cols <- which(names(data) %in% c(
    "EnvironmentalImpact", "CurrentImpact", "EATImpact", "Difference", "Divisor"
  ))
  if (length(impact_cols) > 0L && nrow(data) > 0L) {
    addStyle(
      wb, sheet_name, sci_style,
      rows = 2:(nrow(data) + 1L), cols = impact_cols, gridExpand = TRUE
    )
  }
  percent_cols <- which(names(data) %in% c("ChangePct", "ContributionPct"))
  if (length(percent_cols) > 0L && nrow(data) > 0L) {
    addStyle(
      wb, sheet_name, percent_style,
      rows = 2:(nrow(data) + 1L), cols = percent_cols, gridExpand = TRUE
    )
  }
}

write_table_sheet("Environment_Info", environment_output)
write_table_sheet("Global_Comparison", global_comparison)
write_table_sheet("Global_Scenario", global_scenario)
write_table_sheet("Income_Comparison", income_comparison)
write_table_sheet("Income_Scenario", income_scenario)
write_table_sheet("Country_Comparison", country_comparison)
write_table_sheet("Country_Scenario", country_scenario)
write_table_sheet("Global_Food", global_food)
write_table_sheet("Income_Food", income_food)
write_table_sheet("Country_Food_Detail", impact_detail)
write_table_sheet("QA", qa)

saveWorkbook(wb, output_path, overwrite = TRUE)

message("Environmental-impact calculation complete: ", output_path)
message(
  "Rows: detail=", nrow(impact_detail),
  "; country scenario=", nrow(country_scenario),
  "; global comparison=", nrow(global_comparison)
)
