library(dplyr)
library(gdxrrw)
library(openxlsx)
library(readxl)
library(tidyr)

# Paths -------------------------------------------------------------------

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
health_dir <- file.path(project_dir, "Health model")
gdx_file <- file.path(
  project_dir,
  "food_proxy_code_data_reps",
  "output_reps_full",
  "FBS_proxy_2020.gdx"
)
gams_dir <- "/Library/Frameworks/GAMS.framework/Versions/49/Resources"
analysis_year <- 2020

dir.create(health_dir, recursive = TRUE, showWarnings = FALSE)

input_files <- c(
  "00New_FML_age_groups.xlsx",
  "00New_MLE_age_groups.xlsx",
  "00Ori_FML_age_groups.xlsx",
  "00Ori_MLE_age_groups.xlsx"
)
input_sexes <- c("FML", "MLE", "FML", "MLE")

required_files <- c(gdx_file, file.path(health_dir, input_files))
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing files: ", paste(missing_files, collapse = ", "))
}

# Groups ------------------------------------------------------------------

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80+"
)
sheet_names <- ifelse(age_levels == "80+", "80plus", age_levels)

grain_foods <- c(
  "wheat", "rice", "maize", "barley", "rye",
  "oats", "millet", "sorghum", "othr_grains"
)
red_meat_foods <- c("beef", "lamb", "pork")
grain_prcd_foods <- c("whole_grains", "prc_grains")
red_meat_prcd_foods <- c("red_meat", "prc_meat")
milk_prcd_foods <- c("yoghurt", "cheese", "milk_actl")
target_prcd_foods <- c(
  grain_prcd_foods,
  red_meat_prcd_foods,
  milk_prcd_foods
)

# Input -------------------------------------------------------------------

read_prim_book <- function(file, sex) {
  missing_sheets <- setdiff(sheet_names, excel_sheets(file))

  if (length(missing_sheets) > 0) {
    stop(
      basename(file),
      " missing sheets: ",
      paste(missing_sheets, collapse = ", ")
    )
  }

  data <- Map(function(sheet, age) {
    read_excel(file, sheet = sheet) |>
      transmute(
        type = as.character(type),
        unit = as.character(unit),
        food_group = as.character(food_group),
        region = as.character(region),
        age = .env$age,
        sex = .env$sex,
        year = .env$analysis_year,
        stats = as.character(stats),
        value = replace_na(as.numeric(value), 0)
      )
  }, sheet_names, age_levels)

  bind_rows(data) |>
    filter(
      type == "prim",
      unit == "g/d_w",
      stats == "mean"
    )
}

read_gdx_reference <- function() {
  igdx(gams_dir)

  rgdx.param(
    gdxName = gdx_file,
    symName = "FBS_intake_age_sex_agg",
    names = c(
      "type", "unit", "food_group", "region",
      "age", "sex", "year", "stats"
    ),
    compress = TRUE
  ) |>
    transmute(
      type = as.character(type),
      unit = as.character(unit),
      food_group = as.character(food_group),
      region = as.character(region),
      age = as.character(age),
      sex = as.character(sex),
      year = analysis_year,
      stats = as.character(stats),
      value = as.numeric(value)
    ) |>
    filter(
      type %in% c("prim", "prcd"),
      unit == "g/d_w",
      stats == "mean",
      sex %in% c("FML", "MLE"),
      age %in% age_levels
    )
}

# Reference shares --------------------------------------------------------

build_reference_shares <- function(reference_data) {
  reference_prim <- reference_data |>
    filter(
      type == "prim",
      food_group %in% c(grain_foods, red_meat_foods, "milk")
    )

  actual_prcd <- reference_data |>
    filter(
      type == "prcd",
      food_group %in% target_prcd_foods
    )

  grain_base <- reference_prim |>
    filter(food_group %in% grain_foods) |>
    group_by(region, age, sex, year) |>
    summarise(base_value = sum(value, na.rm = TRUE), .groups = "drop")

  red_meat_base <- reference_prim |>
    filter(food_group %in% red_meat_foods) |>
    group_by(region, age, sex, year) |>
    summarise(base_value = sum(value, na.rm = TRUE), .groups = "drop")

  milk_base <- reference_prim |>
    filter(food_group == "milk") |>
    transmute(region, age, sex, year, base_value = value)

  grain_shares <- actual_prcd |>
    filter(food_group %in% grain_prcd_foods) |>
    left_join(grain_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "total_grains",
      share = if_else(
        !is.na(base_value) & base_value > 0,
        value / base_value,
        0
      )
    )

  red_meat_shares <- actual_prcd |>
    filter(food_group %in% red_meat_prcd_foods) |>
    left_join(red_meat_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "total_red_meat",
      share = if_else(
        !is.na(base_value) & base_value > 0,
        value / base_value,
        0
      )
    )

  milk_shares <- actual_prcd |>
    filter(food_group %in% milk_prcd_foods) |>
    left_join(milk_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "milk",
      share = if_else(
        !is.na(base_value) & base_value > 0,
        value / base_value,
        0
      )
    )

  bind_rows(grain_shares, red_meat_shares, milk_shares) |>
    select(region, age, sex, year, food_group, source_group, share)
}

# Reconstruction ---------------------------------------------------------

reconstruct_prcd <- function(prim_data, share_data) {
  grain_base <- prim_data |>
    filter(food_group %in% grain_foods) |>
    group_by(region, age, sex, year) |>
    summarise(source_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    mutate(source_group = "total_grains")

  red_meat_base <- prim_data |>
    filter(food_group %in% red_meat_foods) |>
    group_by(region, age, sex, year) |>
    summarise(source_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    mutate(source_group = "total_red_meat")

  milk_base <- prim_data |>
    filter(food_group == "milk") |>
    transmute(
      region,
      age,
      sex,
      year,
      source_value = value,
      source_group = "milk"
    )

  bind_rows(grain_base, red_meat_base, milk_base) |>
    inner_join(
      share_data,
      by = c("region", "age", "sex", "year", "source_group")
    ) |>
    transmute(
      type = "prcd",
      unit = "g/d_w",
      food_group,
      region,
      age,
      sex,
      year,
      stats = "mean",
      value = replace_na(source_value * share, 0)
    )
}

write_age_book <- function(data, file) {
  workbook <- createWorkbook()

  for (index in seq_along(age_levels)) {
    age_data <- data |>
      filter(age == age_levels[index]) |>
      arrange(region, food_group)

    addWorksheet(workbook, sheet_names[index])
    writeData(workbook, sheet_names[index], age_data)
  }

  saveWorkbook(workbook, file, overwrite = TRUE)
}

# Run ---------------------------------------------------------------------

share_data <- read_gdx_reference() |>
  build_reference_shares()

for (index in seq_along(input_files)) {
  input_file <- file.path(health_dir, input_files[index])
  output_file <- file.path(
    health_dir,
    sub("\\.xlsx$", "_prcd.xlsx", input_files[index])
  )

  read_prim_book(input_file, input_sexes[index]) |>
    reconstruct_prcd(share_data) |>
    write_age_book(output_file)

  message("Saved: ", output_file)
}
