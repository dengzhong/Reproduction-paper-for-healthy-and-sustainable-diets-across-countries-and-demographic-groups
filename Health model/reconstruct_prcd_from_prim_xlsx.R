library(dplyr)
library(gdxrrw)
library(openxlsx)
library(purrr)
library(readr)
library(readxl)
library(tidyr)

analysis_year <- 2020
gams_sysdir <- "/Library/Frameworks/GAMS.framework/Versions/49/Resources"
gdx_file <- "/Users/zhongcideng/Desktop/FoodBalance2013-/food_proxy_code_data_reps/output_reps_full/FBS_proxy_2020.gdx"

input_books <- c(
  FML = "/Users/zhongcideng/Desktop/FoodBalance2013-/optimized/optmized/output/Health model/00New_FML_age_groups.xlsx",
  MLE = "/Users/zhongcideng/Desktop/FoodBalance2013-/optimized/optmized/output/Health model/00New_MLE_age_groups.xlsx"
)

output_dir <- "/Users/zhongcideng/Desktop/FoodBalance2013-/optimized/optmized/output/Health model"
ratio_file <- file.path(output_dir, paste0("prcd_reference_shares_", analysis_year, ".csv"))
recon_file <- file.path(output_dir, paste0("prcd_reconstructed_", analysis_year, ".csv"))
check_file <- file.path(output_dir, paste0("prcd_reconstruction_check_", analysis_year, ".csv"))
summary_file <- file.path(output_dir, paste0("prcd_reconstruction_summary_", analysis_year, ".csv"))

grain_foods <- c("wheat", "rice", "maize", "barley", "rye", "oats", "millet", "sorghum", "othr_grains")
red_meat_foods <- c("beef", "lamb", "pork")
milk_prcd_foods <- c("yoghurt", "cheese", "milk_actl")
grain_prcd_foods <- c("whole_grains", "prc_grains")
red_meat_prcd_foods <- c("red_meat", "prc_meat")
target_prcd_foods <- c(grain_prcd_foods, red_meat_prcd_foods,milk_prcd_foods)

read_age_book <- function(path, sex_name) {
  map_dfr(excel_sheets(path), function(sheet_name) {
    read_excel(path, sheet = sheet_name) |>
      mutate(sheet_age = sheet_name)
  }) |>
    mutate(
      type = as.character(type),
      unit = as.character(unit),
      food_group = as.character(food_group),
      region = as.character(region),
      age = as.character(age),
      sex = sex_name,
      stats = as.character(stats),
      year = analysis_year,
      value = as.numeric(value)
    )
}

read_prim_input <- function(book_map) {
  imap_dfr(book_map, read_age_book) |>
    filter(
      type == "prim",
      unit == "g/d_w",
      stats == "mean",
      sex != "BTH",
      age != "all-a"
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value)
}

read_actual_prcd <- function() {
  igdx(gams_sysdir)

  rgdx.param(
    gdxName = gdx_file,
    symName = "FBS_intake_age_sex_agg",
    names = c("type", "unit", "food_group", "region", "age", "sex", "year", "stats"),
    compress = TRUE
  ) |>
    mutate(
      type = as.character(type),
      unit = as.character(unit),
      food_group = as.character(food_group),
      region = as.character(region),
      age = as.character(age),
      sex = as.character(sex),
      stats = as.character(stats),
      year = analysis_year
    ) |>
    filter(
      type == "prcd",
      unit == "g/d_w",
      stats == "mean",
      sex != "BTH",
      age != "all-a",
      food_group %in% target_prcd_foods
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value)
}

read_reference_prim <- function() {
  igdx(gams_sysdir)

  rgdx.param(
    gdxName = gdx_file,
    symName = "FBS_intake_age_sex_agg",
    names = c("type", "unit", "food_group", "region", "age", "sex", "year", "stats"),
    compress = TRUE
  ) |>
    mutate(
      type = as.character(type),
      unit = as.character(unit),
      food_group = as.character(food_group),
      region = as.character(region),
      age = as.character(age),
      sex = as.character(sex),
      stats = as.character(stats),
      year = analysis_year
    ) |>
    filter(
      type == "prim",
      unit == "g/d_w",
      stats == "mean",
      sex != "BTH",
      age != "all-a",
      food_group %in% c(grain_foods, red_meat_foods, "milk")
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value)
}

build_reference_shares <- function(reference_prim, actual_prcd) {
  # The numerator and denominator must come from the same GDX reference data.
  # These shares are subsequently applied to the (possibly changed) Excel prim data.
  grain_base <- reference_prim |>
    filter(food_group %in% grain_foods) |>
    group_by(region, age, sex, year) |>
    summarise(base_value = sum(value), .groups = "drop")

  red_meat_base <- reference_prim |>
    filter(food_group %in% red_meat_foods) |>
    group_by(region, age, sex, year) |>
    summarise(base_value = sum(value), .groups = "drop")

  milk_base <- reference_prim |>
    filter(food_group == "milk") |>
    transmute(region, age, sex, year, base_value = value)

  grain_shares <- actual_prcd |>
    filter(food_group %in% grain_prcd_foods) |>
    left_join(grain_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "total_grains",
      share = if_else(!is.na(base_value) & base_value > 0, value / base_value, 0)
    )

  red_meat_shares <- actual_prcd |>
    filter(food_group %in% red_meat_prcd_foods) |>
    left_join(red_meat_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "total_red_meat",
      share = if_else(!is.na(base_value) & base_value > 0, value / base_value, 0)
    )

  milk_shares <- actual_prcd |>
    filter(food_group %in% milk_prcd_foods) |>
    left_join(milk_base, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_group = "milk",
      share = if_else(!is.na(base_value) & base_value > 0, value / base_value, 0)
    )

  bind_rows(grain_shares, red_meat_shares, milk_shares) |>
    select(region, age, sex, year, food_group, source_group, share)
}

reconstruct_prcd <- function(prim_data, share_data) {
  grain_base <- prim_data |>
    filter(food_group %in% grain_foods) |>
    group_by(region, age, sex, year) |>
    summarise(source_value = sum(value), .groups = "drop") |>
    mutate(source_group = "total_grains")

  red_meat_base <- prim_data |>
    filter(food_group %in% red_meat_foods) |>
    group_by(region, age, sex, year) |>
    summarise(source_value = sum(value), .groups = "drop") |>
    mutate(source_group = "total_red_meat")

  milk_base <- prim_data |>
    filter(food_group == "milk") |>
    transmute(region, age, sex, year, source_value = value, source_group = "milk")

  base_values <- bind_rows(grain_base, red_meat_base, milk_base)

  # Use the New FML/MLE prim data as the master table. This prevents countries,
  # ages or sex groups that exist only in the GDX reference from entering output.
  base_values |>
    inner_join(
      share_data,
      by = c("region", "age", "sex", "year", "source_group")
    ) |>
    mutate(
      source_value = coalesce(source_value, 0),
      value = source_value * share,
      type = "prcd",
      unit = "g/d_w",
      stats = "mean"
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value) |>
    arrange(sex, age, region, food_group)
}

compare_prcd <- function(reconstructed, actual_prcd) {
  actual_prcd |>
    rename(actual_value = value) |>
    full_join(
      reconstructed |> rename(reconstructed_value = value),
      by = c("type", "unit", "food_group", "region", "age", "sex", "year", "stats")
    ) |>
    mutate(
      reconstructed_value = coalesce(reconstructed_value, 0),
      actual_value = coalesce(actual_value, 0),
      diff = reconstructed_value - actual_value,
      abs_diff = abs(diff),
      rel_diff = if_else(actual_value != 0, diff / actual_value, NA_real_)
    )
}

write_age_workbooks <- function(data, prefix) {
  split(data, data$sex) |>
    iwalk(function(sex_data, sex_name) {
      wb <- createWorkbook()

      age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

      for (age_i in age_levels) {
        age_data <- sex_data |>
          filter(age == age_i) |>
          arrange(region, food_group)

        if (nrow(age_data) == 0) {
          next
        }

        sheet_name <- if (age_i == "80+") "80plus" else age_i
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, age_data)
      }

      saveWorkbook(
        wb,
        file = file.path(output_dir, paste0(sex_name, "_", prefix, ".xlsx")),
        overwrite = TRUE
      )
    })
}

prim_input <- read_prim_input(input_books)

# Only retain countries that occur in New_FML_age_groups.xlsx or
# New_MLE_age_groups.xlsx. The sex/age-specific restriction is enforced again
# by the inner_join in reconstruct_prcd().
allowed_regions <- prim_input |>
  distinct(region)

actual_prcd <- read_actual_prcd() |>
  semi_join(allowed_regions, by = "region")

reference_prim <- read_reference_prim() |>
  semi_join(allowed_regions, by = "region")

share_data <- build_reference_shares(reference_prim, actual_prcd)
reconstructed_prcd <- reconstruct_prcd(prim_input, share_data)
comparison <- compare_prcd(reconstructed_prcd, actual_prcd)

summary_tbl <- comparison |>
  group_by(food_group) |>
  summarise(
    n = n(),
    mae = mean(abs_diff, na.rm = TRUE),
    rmse = sqrt(mean(diff^2, na.rm = TRUE)),
    max_abs_diff = max(abs_diff, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(food_group)

write_csv(share_data, ratio_file)
write_csv(reconstructed_prcd, recon_file)
write_csv(comparison, check_file)
write_csv(summary_tbl, summary_file)

write_age_workbooks(reconstructed_prcd, "age_groups_prcd_reconstructed")

cat("Saved reference shares to:", ratio_file, "\n")
cat("Saved reconstructed prcd data to:", recon_file, "\n")
cat("Saved reconstruction check to:", check_file, "\n")
cat("Saved reconstruction summary to:", summary_file, "\n")
print(summary_tbl)
