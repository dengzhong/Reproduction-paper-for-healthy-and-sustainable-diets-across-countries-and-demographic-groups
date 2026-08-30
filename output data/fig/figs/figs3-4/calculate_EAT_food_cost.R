#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# Resolve paths from the script location so the script can be run from any folder.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) == 1L) {
  normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
} else {
  normalizePath("calculate_EAT_food_cost.R", mustWork = TRUE)
}
data_dir <- dirname(script_path)

input_paths <- c(
  demand = file.path(data_dir, "sensitivity-EAT-Lancet-demand.xlsx"),
  population = file.path(data_dir, "population_long.xlsx"),
  price = file.path(data_dir, "price.xlsx"),
  waste = file.path(data_dir, "waste_coff.xlsx"),
  income = file.path(data_dir, "Income-level.xlsx")
)
output_path <- file.path(data_dir, "sensitivity-EAT-Lancet-food-cost.xlsx")

missing_files <- input_paths[!file.exists(input_paths)]
if (length(missing_files) > 0L) {
  stop("Missing input file(s): ", paste(missing_files, collapse = ", "))
}

current_wide <- read_excel(input_paths[["demand"]], sheet = "Demand_country")
eat_wide <- read_excel(input_paths[["demand"]], sheet = "Demand_country_EAT")
price_wide <- read_excel(input_paths[["price"]])
waste_wide <- read_excel(input_paths[["waste"]])
population_wide <- read_excel(input_paths[["population"]])
income <- read_excel(input_paths[["income"]])

# Standardize the country identifier while preserving all food names as supplied.
names(current_wide)[1] <- "ISO3"
names(eat_wide)[1] <- "ISO3"
names(price_wide)[1] <- "ISO3"
names(waste_wide)[1] <- "ISO3"
names(population_wide)[1] <- "ISO3"
names(income)[1:2] <- c("ISO3", "IncomeLevel")

food_cols <- setdiff(names(current_wide), "ISO3")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_true(identical(food_cols, setdiff(names(eat_wide), "ISO3")),
            "Current and EAT demand food columns do not match.")
assert_true(identical(food_cols, setdiff(names(price_wide), "ISO3")),
            "Demand and price food columns do not match.")
assert_true(identical(food_cols, setdiff(names(waste_wide), "ISO3")),
            "Demand and waste food columns do not match.")

country_sets <- list(
  current = sort(current_wide$ISO3),
  eat = sort(eat_wide$ISO3),
  price = sort(price_wide$ISO3),
  waste = sort(waste_wide$ISO3),
  population = sort(unique(population_wide$ISO3)),
  income = sort(income$ISO3)
)
assert_true(all(vapply(country_sets[-1], identical, logical(1), country_sets[[1]])),
            "ISO3 country coverage is not identical across the five input workbooks.")
assert_true(!anyDuplicated(current_wide$ISO3), "Duplicate ISO3 values in current demand.")
assert_true(!anyDuplicated(eat_wide$ISO3), "Duplicate ISO3 values in EAT demand.")
assert_true(!anyDuplicated(price_wide$ISO3), "Duplicate ISO3 values in price data.")
assert_true(!anyDuplicated(waste_wide$ISO3), "Duplicate ISO3 values in waste data.")
assert_true(!anyDuplicated(income$ISO3), "Duplicate ISO3 values in income mapping.")

to_food_long <- function(data, value_name) {
  data %>%
    pivot_longer(
      cols = all_of(food_cols),
      names_to = "Food",
      values_to = value_name
    )
}

demand_long <- bind_rows(
  to_food_long(current_wide, "Demand") %>% mutate(Scenario = "Current"),
  to_food_long(eat_wide, "Demand") %>% mutate(Scenario = "EAT")
) %>%
  select(Scenario, ISO3, Food, Demand)

price_long <- to_food_long(price_wide, "Price")
waste_long <- to_food_long(waste_wide, "WasteCoefficient")

food_cost_detail <- demand_long %>%
  left_join(price_long, by = c("ISO3", "Food")) %>%
  left_join(waste_long, by = c("ISO3", "Food"))

assert_true(!anyNA(food_cost_detail[c("Demand", "Price", "WasteCoefficient")]),
            "Missing demand, price, or waste value after joining by ISO3 and food.")
assert_true(all(food_cost_detail$WasteCoefficient > 0),
            "Waste coefficients must be greater than zero because cost divides by waste.")
assert_true(all(food_cost_detail$Demand >= 0), "Demand contains negative values.")
assert_true(all(food_cost_detail$Price >= 0), "Price contains negative values.")

# User-specified accounting identity. WasteCoefficient is used directly as a divisor.
food_cost_detail <- food_cost_detail %>%
  mutate(FoodCostPerCapitaDay = Demand / WasteCoefficient * Price)

# Sum all age groups and both sexes to obtain one population total per country.
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

country_scenario <- food_cost_detail %>%
  group_by(Scenario, ISO3) %>%
  summarise(
    FoodCostPerCapitaDay = sum(FoodCostPerCapitaDay),
    .groups = "drop"
  ) %>%
  left_join(population_country, by = "ISO3") %>%
  left_join(income, by = "ISO3")

assert_true(!anyNA(country_scenario[c("Population", "IncomeLevel")]),
            "Population or income level is missing after country join.")

country_summary <- country_scenario %>%
  select(ISO3, IncomeLevel, Population, Scenario, FoodCostPerCapitaDay) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = FoodCostPerCapitaDay,
    names_glue = "{Scenario}CostPerCapitaDay"
  ) %>%
  mutate(
    Difference = EATCostPerCapitaDay - CurrentCostPerCapitaDay,
    ChangePct = if_else(
      CurrentCostPerCapitaDay == 0,
      NA_real_,
      100 * Difference / CurrentCostPerCapitaDay
    )
  ) %>%
  arrange(ISO3)

weighted_summary <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars)), Scenario) %>%
    summarise(
      Countries = n_distinct(ISO3),
      WeightedCostNumerator = sum(FoodCostPerCapitaDay * Population),
      Population = sum(Population),
      .groups = "drop"
    ) %>%
    mutate(
      PopulationWeightedFoodCostPerCapitaDay = WeightedCostNumerator / Population
    ) %>%
    select(-WeightedCostNumerator)
}

income_scenario <- weighted_summary(country_scenario, "IncomeLevel")
global_scenario <- country_scenario %>%
  mutate(Geography = "World") %>%
  weighted_summary("Geography")

make_comparison <- function(data, id_cols) {
  data %>%
    pivot_wider(
      names_from = Scenario,
      values_from = PopulationWeightedFoodCostPerCapitaDay,
      names_glue = "{Scenario}CostPerCapitaDay"
    ) %>%
    mutate(
      Difference = EATCostPerCapitaDay - CurrentCostPerCapitaDay,
      ChangePct = if_else(
        CurrentCostPerCapitaDay == 0,
        NA_real_,
        100 * Difference / CurrentCostPerCapitaDay
      )
    ) %>%
    relocate(all_of(id_cols))
}

income_summary <- make_comparison(income_scenario, c("IncomeLevel", "Countries", "Population")) %>%
  arrange(factor(
    IncomeLevel,
    levels = c("Low income", "Lower middle income", "Upper-middle income", "High income")
  ))

global_summary <- make_comparison(global_scenario, c("Geography", "Countries", "Population"))

# Preserve food-level structure in the income-group and global per-capita results.
food_cost_detail_weighted <- food_cost_detail %>%
  left_join(population_country, by = "ISO3") %>%
  left_join(income, by = "ISO3")

income_food_detail <- food_cost_detail_weighted %>%
  group_by(IncomeLevel, Scenario, Food) %>%
  summarise(
    Countries = n_distinct(ISO3),
    WeightedCostNumerator = sum(FoodCostPerCapitaDay * Population),
    Population = sum(Population),
    .groups = "drop"
  ) %>%
  mutate(
    PopulationWeightedFoodCostPerCapitaDay = WeightedCostNumerator / Population
  ) %>%
  select(-WeightedCostNumerator) %>%
  group_by(IncomeLevel, Scenario) %>%
  mutate(
    ContributionPct = 100 * PopulationWeightedFoodCostPerCapitaDay /
      sum(PopulationWeightedFoodCostPerCapitaDay)
  ) %>%
  ungroup() %>%
  arrange(IncomeLevel, Scenario, desc(PopulationWeightedFoodCostPerCapitaDay), Food)

global_food_detail <- food_cost_detail_weighted %>%
  group_by(Scenario, Food) %>%
  summarise(
    Geography = "World",
    Countries = n_distinct(ISO3),
    WeightedCostNumerator = sum(FoodCostPerCapitaDay * Population),
    Population = sum(Population),
    .groups = "drop"
  ) %>%
  mutate(
    PopulationWeightedFoodCostPerCapitaDay = WeightedCostNumerator / Population
  ) %>%
  select(-WeightedCostNumerator) %>%
  group_by(Scenario) %>%
  mutate(
    ContributionPct = 100 * PopulationWeightedFoodCostPerCapitaDay /
      sum(PopulationWeightedFoodCostPerCapitaDay)
  ) %>%
  ungroup() %>%
  select(Geography, Scenario, Food, Countries, Population,
         PopulationWeightedFoodCostPerCapitaDay, ContributionPct) %>%
  arrange(Scenario, desc(PopulationWeightedFoodCostPerCapitaDay), Food)

qa <- tibble(
  Check = c(
    "Countries in every input",
    "Food items in every demand/price/waste table",
    "Detailed scenario-country-food rows",
    "Sex-age population rows",
    "Population total",
    "Waste coefficients <= 0",
    "Missing joined calculation values",
    "Calculation formula"
  ),
  Result = c(
    length(country_sets[[1]]),
    length(food_cols),
    nrow(food_cost_detail),
    nrow(population_wide),
    format(sum(population_country$Population), scientific = FALSE, trim = TRUE),
    sum(food_cost_detail$WasteCoefficient <= 0),
    sum(is.na(food_cost_detail$FoodCostPerCapitaDay)),
    "Demand / WasteCoefficient * Price"
  )
)

readme <- data.frame(
  Item = c(
    "Purpose",
    "Country-level formula",
    "Country population",
    "Income/global per-capita aggregation",
    "Food-level detail",
    "Demand source",
    "Population source",
    "Price source",
    "Waste source",
    "Income-group source"
  ),
  Description = c(
    "Current and EAT-Lancet food-cost estimates at country, income-group and global levels.",
    "FoodCost(i,j) = Demand(i,j) / WasteCoefficient(i,j) * Price(i,j); summed over foods for each country.",
    "All age-group columns are summed across both sex rows for each ISO3.",
    "sum(country per-capita food cost * country population) / sum(country population).",
    "Food-cost inputs and results are retained by scenario, ISO3 and food; income/global food details are population-weighted by food.",
    basename(input_paths[["demand"]]),
    basename(input_paths[["population"]]),
    basename(input_paths[["price"]]),
    basename(input_paths[["waste"]]),
    basename(input_paths[["income"]])
  ),
  stringsAsFactors = FALSE
)

# Workbook export with compact, publication-supporting formatting.
wb <- createWorkbook(creator = "Codex")
header_style <- createStyle(
  fontName = "Arial", fontSize = 10, textDecoration = "bold",
  fgFill = "#DCE6F1", border = "Bottom", borderColour = "#7F7F7F",
  halign = "center", valign = "center", wrapText = TRUE
)
body_style <- createStyle(fontName = "Arial", fontSize = 9, valign = "center")
decimal_style <- createStyle(numFmt = "0.000000")
percent_style <- createStyle(numFmt = "0.00%")
integer_style <- createStyle(numFmt = "#,##0")
large_style <- createStyle(numFmt = "#,##0.00")

write_sheet <- function(sheet_name, data, freeze_col = 1L) {
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  writeData(wb, sheet_name, data, withFilter = TRUE, headerStyle = header_style)
  addStyle(wb, sheet_name, body_style,
           rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
  freezePane(wb, sheet_name, firstRow = TRUE, firstCol = freeze_col > 0L)
  setColWidths(wb, sheet_name, cols = 1:ncol(data), widths = "auto")
  current_widths <- pmin(30, pmax(10, nchar(names(data)) + 2))
  setColWidths(wb, sheet_name, cols = seq_along(current_widths), widths = current_widths)
}

write_sheet("README", readme, 0L)
setColWidths(wb, "README", cols = 1, widths = 34)
setColWidths(wb, "README", cols = 2, widths = 95)
setRowHeights(wb, "README", rows = 2:(nrow(readme) + 1), heights = 30)
addStyle(wb, "README", createStyle(wrapText = TRUE, valign = "top"),
         rows = 2:(nrow(readme) + 1), cols = 1:2, gridExpand = TRUE)

write_sheet("Global_summary", global_summary)
write_sheet("Income_summary", income_summary)
write_sheet("Country_summary", country_summary)
write_sheet("Country_scenario", country_scenario)
write_sheet("Global_food_detail", global_food_detail)
write_sheet("Income_food_detail", income_food_detail)
write_sheet("Food_cost_detail", food_cost_detail)
write_sheet("QA", qa, 0L)
setColWidths(wb, "QA", cols = 1, widths = 44)
setColWidths(wb, "QA", cols = 2, widths = 38)

# Apply practical number formats by column-name pattern.
for (sheet in c("Global_summary", "Income_summary", "Country_summary", "Country_scenario",
                "Global_food_detail", "Income_food_detail")) {
  data <- switch(
    sheet,
    Global_summary = global_summary,
    Income_summary = income_summary,
    Country_summary = country_summary,
    Country_scenario = country_scenario,
    Global_food_detail = global_food_detail,
    Income_food_detail = income_food_detail
  )
  n <- nrow(data) + 1L
  pop_cols <- grep("^(Population|Countries)$", names(data))
  pct_cols <- grep("ChangePct$", names(data))
  value_cols <- grep("Cost|Difference", names(data))
  if (length(pop_cols)) addStyle(wb, sheet, integer_style, rows = 2:n, cols = pop_cols, gridExpand = TRUE, stack = TRUE)
  if (length(value_cols)) addStyle(wb, sheet, large_style, rows = 2:n, cols = value_cols, gridExpand = TRUE, stack = TRUE)
  if (length(pct_cols)) addStyle(wb, sheet, createStyle(numFmt = "0.00"), rows = 2:n, cols = pct_cols, gridExpand = TRUE, stack = TRUE)
}

detail_n <- nrow(food_cost_detail) + 1L
detail_num_cols <- match(c("Demand", "Price", "WasteCoefficient", "FoodCostPerCapitaDay"), names(food_cost_detail))
addStyle(wb, "Food_cost_detail", decimal_style,
         rows = 2:detail_n, cols = detail_num_cols, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, output_path, overwrite = TRUE)

cat("Created:", output_path, "\n")
cat("Countries:", nrow(country_summary), "\n")
cat("Food items:", length(food_cols), "\n")
cat("Population:", format(sum(population_country$Population), scientific = FALSE), "\n")
cat("Global current per-capita daily cost:",
    global_summary$CurrentCostPerCapitaDay, "\n")
cat("Global EAT-Lancet per-capita daily cost:",
    global_summary$EATCostPerCapitaDay, "\n")
