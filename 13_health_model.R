args <- commandArgs(trailingOnly = TRUE)

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
base_dir <- file.path(project_dir, "Health model")
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

step1_output_name <- if (length(args) >= 2) args[2] else "health_food_intake_step1.xlsx"
step2_output_name <- if (length(args) >= 3) args[3] else "health_food_pif_step2.xlsx"
step3_output_name <- if (length(args) >= 4) args[4] else "health_food_deaths_step3.xlsx"
step1_output_path <- file.path(base_dir, step1_output_name)
step2_output_path <- file.path(base_dir, step2_output_name)
step3_output_path <- file.path(base_dir, step3_output_name)

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

write_workbook <- function(sheets, file) {
  workbook <- createWorkbook()

  iwalk(sheets, function(data, sheet) {
    addWorksheet(workbook, sheet)
    writeData(workbook, sheet, data)
  })

  saveWorkbook(workbook, file, overwrite = TRUE)
}

target_categories <- c(
  "Processed meat", "Red meat", "Fruits", "Vegetables",
  "Whole grains",
  "Legumes", "Nuts"
)

# The three input files use different labels for the same age groups.
age_map <- tibble::tribble(
  ~intake_age, ~population_age, ~death_rate_age,
  "20-24",     "20-24",         "20-24 years",
  "25-29",     "25-29",         "25-29 years",
  "30-34",     "30-34",         "30-34 years",
  "35-39",     "35-39",         "35-39 years",
  "40-44",     "40-44",         "40-44 years",
  "45-49",     "45-49",         "45-49 years",
  "50-54",     "50-54",         "50-54 years",
  "55-59",     "55-59",         "55-59 years",
  "60-64",     "60-64",         "60-64 years",
  "65-69",     "65-69",         "65-69 years",
  "70-74",     "70-74",         "70-74 years",
  "75-79",     "75-79",         "75-79 years",
  "80plus",    "80+",           "80+ years"
)

# Intake/population use FML/MLE, while Death rate.xlsx uses Female/Male.
sex_map <- tibble::tribble(
  ~Sex,  ~death_rate_sex,
  "FML", "Female",
  "MLE", "Male"
)

ori_path <- file.path(base_dir, "00Intake_Ori.xlsx")
opt_path <- file.path(base_dir, "00Intake_Opt.xlsx")
risk_food_path <- file.path(base_dir, "food factor.xlsx")
risk_path <- file.path(base_dir, "risk factor.xlsx")
death_rate_path <- file.path(base_dir, "Death rate.xlsx")
population_path <- file.path(base_dir, "population.xlsx")

required_files <- c(
  ori_path, opt_path, risk_food_path, risk_path,
  death_rate_path, population_path
)
if (any(!file.exists(required_files))) {
  stop("Missing input file(s): ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

# Food-to-health-category mapping is maintained in food factor.xlsx.
category_map_df <- read_excel(
  risk_food_path,
  sheet = 1,
  .name_repair = "minimal"
) %>%
  select(Food, `Food category`) %>%
  filter(
    !is.na(Food),
    !is.na(`Food category`),
    `Food category` %in% target_categories
  ) %>%
  transmute(
    food_group = as.character(Food),
    `Food category` = as.character(`Food category`)
  ) %>%
  # Processed grains are handled differently for Ori and Opt below.
  filter(food_group != "prc_grains") %>%
  distinct()

# Ori: processed grains are not Whole grains and are excluded from the health
# categories. Opt: processed grains are counted as Whole grains.
category_map_ori_df <- category_map_df
category_map_opt_df <- bind_rows(
  category_map_df,
  tibble::tibble(
    food_group = "prc_grains",
    `Food category` = "Whole grains"
  )
) %>%
  distinct()

missing_categories <- setdiff(target_categories, category_map_df$`Food category`)
if (length(missing_categories) > 0) {
  stop(
    "food factor.xlsx has no mapping for health category/categories: ",
    paste(missing_categories, collapse = ", ")
  )
}

# Step 1: Intake ----------------------------------------------------------

# Intake calculation: formula unchanged; data are now long-format and sex-specific.
read_and_aggregate_intake <- function(path, suffix, scenario_category_map) {
  available_sheets <- excel_sheets(path)
  missing_sheets <- setdiff(age_map$intake_age, available_sheets)
  if (length(missing_sheets) > 0) {
    stop("Missing age sheet(s) in ", basename(path), ": ", paste(missing_sheets, collapse = ", "))
  }

  map_dfr(age_map$intake_age, function(age_sheet) {
    intake_df <- read_excel(path, sheet = age_sheet, .name_repair = "minimal")
    required_cols <- c("food_group", "region", "age", "Sex", "value")
    missing_cols <- setdiff(required_cols, names(intake_df))
    if (length(missing_cols) > 0) {
      stop("Missing column(s) in ", basename(path), "/", age_sheet, ": ", paste(missing_cols, collapse = ", "))
    }

    intake_df %>%
      filter(Sex %in% sex_map$Sex) %>%
      inner_join(scenario_category_map, by = "food_group") %>%
      group_by(ISO3 = region, Sex, age_group = age_sheet, `Food category`) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      complete(
        nesting(ISO3, Sex, age_group),
        `Food category` = target_categories,
        fill = list(value = 0)
      ) %>%
      pivot_wider(names_from = `Food category`, values_from = value) %>%
      mutate(total_5groups = rowSums(pick(all_of(target_categories)), na.rm = TRUE)) %>%
      rename_with(
        ~ paste0(.x, suffix),
        all_of(c(target_categories, "total_5groups"))
      )
  })
}

ori_detail <- read_and_aggregate_intake(
  ori_path,
  "_Ori",
  category_map_ori_df
) |>
  filter(ISO3 != "MUS")

opt_detail <- read_and_aggregate_intake(
  opt_path,
  "_Opt",
  category_map_opt_df
)

# Sex is now part of every identification/join key.
step1_detail_df <- inner_join(
  ori_detail,
  opt_detail,
  by = c("ISO3", "Sex", "age_group")
)

step1_age_summary_df <- step1_detail_df %>%
  group_by(Sex, age_group) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

write_workbook(
  list(
    detail = step1_detail_df,
    age_summary = step1_age_summary_df,
    category_map_Ori = category_map_ori_df,
    category_map_Opt = category_map_opt_df
  ),
  step1_output_path
)

# Step 2: Population impact fraction -------------------------------------

step1_detail_df <- read_excel(step1_output_path, sheet = "detail")

# PIF calculation
raw_risk_df <- read_excel(risk_path, col_names = FALSE, .name_repair = "minimal")
names(raw_risk_df) <- c("Food group", "Endpoint", "Unit", "RR_mean", "RR_low", "RR_high")

risk_df <- raw_risk_df %>%
  filter(!is.na(`Food group`) | !is.na(Endpoint)) %>%
  mutate(
    `Food group` = ifelse(`Food group` == "Food group", NA, `Food group`),
    Endpoint = ifelse(Endpoint == "Endpoint", NA, Endpoint)
  ) %>%
  fill(`Food group`) %>%
  filter(!is.na(`Food group`), !is.na(Endpoint), Endpoint != "Endpoint") %>%
  mutate(
    RR_mean = as.numeric(RR_mean),
    Cserv = as.numeric(sub(" .*", "", Unit))
  ) %>%
  select(`Food group`, Endpoint, Unit, Cserv, RR_mean, RR_low, RR_high)

pif_result_list <- list()
result_index <- 1L

for (i in seq_len(nrow(risk_df))) {
  food_group <- risk_df$`Food group`[[i]]
  endpoint <- risk_df$Endpoint[[i]]
  rr_mean <- risk_df$RR_mean[[i]]
  cserv <- risk_df$Cserv[[i]]

  ori_col <- paste0(food_group, "_Ori")
  opt_col <- paste0(food_group, "_Opt")

  if (!(ori_col %in% names(step1_detail_df)) || !(opt_col %in% names(step1_detail_df))) {
    next
  }

  intake_ori <- step1_detail_df[[ori_col]]
  intake_opt <- step1_detail_df[[opt_col]]

  # Formula (6), unchanged.
  pif_value <- 1 - (rr_mean ^ ((intake_opt - intake_ori) / cserv))

  pif_result_list[[result_index]] <- data.frame(
    ISO3 = step1_detail_df$ISO3,
    Sex = step1_detail_df$Sex,
    age_group = step1_detail_df$age_group,
    `Food group` = food_group,
    Endpoint = endpoint,
    Unit = risk_df$Unit[[i]],
    RR_mean = rr_mean,
    Cserv = cserv,
    intake_ori = intake_ori,
    intake_opt = intake_opt,
    PIF = pif_value,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result_index <- result_index + 1L
}

step2_pif_detail_df <- bind_rows(pif_result_list)

write_workbook(
  list(
    risk_factors = risk_df,
    pif_detail = step2_pif_detail_df
  ),
  step2_output_path
)

# Step 3: Deaths ----------------------------------------------------------

# Death calculation
death_rate_df <- read_excel(death_rate_path, .name_repair = "minimal") %>%
  filter(measure_name == "Deaths Rate") %>%
  select(
    ISO3,
    death_rate_sex = sex_name,
    death_rate_age = age_name,
    cause_name,
    year,
    death_rate_per_100k = val
  )

population_df <- read_excel(population_path, .name_repair = "minimal")
required_population_cols <- c("ISO3", "Sex", age_map$population_age)
missing_population_cols <- setdiff(required_population_cols, names(population_df))
if (length(missing_population_cols) > 0) {
  stop("Missing population column(s): ", paste(missing_population_cols, collapse = ", "))
}

population_long_df <- population_df %>%
  select(all_of(required_population_cols)) %>%
  pivot_longer(
    cols = all_of(age_map$population_age),
    names_to = "population_age",
    values_to = "population"
  ) %>%
  inner_join(age_map, by = "population_age") %>%
  select(ISO3, Sex, age_group = intake_age, population)

# Add explicit age/sex labels needed by Death rate.xlsx.
step2_for_deaths_df <- step2_pif_detail_df %>%
  left_join(age_map, by = c("age_group" = "intake_age")) %>%
  left_join(sex_map, by = "Sex")

if (any(is.na(step2_for_deaths_df$death_rate_age))) {
  stop("Some intake age groups have no Death rate age mapping")
}
if (any(is.na(step2_for_deaths_df$death_rate_sex))) {
  stop("Some Sex values have no Death rate sex mapping")
}

step3_detail_df <- step2_for_deaths_df %>%
  left_join(
    death_rate_df,
    by = c(
      "ISO3",
      "death_rate_sex",
      "death_rate_age",
      "Endpoint" = "cause_name"
    )
  ) %>%
  left_join(
    population_long_df,
    by = c("ISO3", "Sex", "age_group")
  ) %>%
  mutate(
    # Formula unchanged.
    delta_deaths = PIF * death_rate_per_100k * population / 100000
  ) %>%
  select(
    ISO3, Sex, age_group, `Food group`, Endpoint, year,
    PIF, death_rate_per_100k, population, delta_deaths
  )

step3_age_summary_df <- step3_detail_df %>%
  group_by(ISO3, Sex, age_group, `Food group`, Endpoint) %>%
  summarise(
    delta_deaths = sum(delta_deaths, na.rm = TRUE),
    .groups = "drop"
  )

write_workbook(
  list(
    delta_deaths_detail = step3_detail_df,
    delta_deaths_summary = step3_age_summary_df
  ),
  step3_output_path
)

cat("Created", step1_output_path, "\n")
cat("Step 1 detail rows:", nrow(step1_detail_df), "\n")
cat("Created", step2_output_path, "\n")
cat("Step 2 PIF detail rows:", nrow(step2_pif_detail_df), "\n")
cat("Created", step3_output_path, "\n")
cat("Step 3 delta deaths rows:", nrow(step3_detail_df), "\n")
